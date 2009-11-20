#!/usr/bin/runhaskell
import Distribution.Franchise.V1
import Data.List ( sort, partition, isPrefixOf, isSuffixOf )
import Data.Char ( toLower )

main = build [configurableProgram "shell" "bash" ["shsh","sh"],
              flag "with-type-witnesses" "for gadt type witnesses"
                (do putS "compiling with type witnesses enabled"
                    define "GADT_WITNESSES"
                    ghcFlags ["-fglasgow-exts"])] $
       do autoPatchVersion NumberedPreRc >>= replace "IOLAUS_VERSION"
          createFile "Iolaus/Help.lhs"
          hcFlags ["-Iinclude"]
          ghcFlags ["-Wall","-threaded"]
          withDirectory "etc" $ etc "bash_completion.d/iolaus"
          withModule "System.Process.Redirects" $ define "HAVE_REDIRECTS"
          executable "iolaus" "iolaus.hs" []
          enforceAllPrivacy
          doc
          allTests

doc =
    do privateExecutable "preproc" "preproc.hs" []
       xs <- (filter (/= "Show.lhs") .
              filter (".lhs" `isSuffixOf`)) `fmap` ls "Iolaus/Commands"
       hs <- mapM commandPage $ xs
       mkdir "manual"
       rule ["manual/manual.md"] ("preproc":"doc/iolaus.md":map fst hs) $
            do x <- systemOut "./preproc" ["doc/iolaus.md"]
               mkFile "manual/manual.md" (prefix "" "Iolaus manual"++x)
       mdToHtml "README.md" "index.html" "Iolaus"
       mdToHtml "TODO.md" "TODO.html" "Iolaus to-do list"
       mdToHtml "doc/FAQ.md" "FAQ.html" "FAQ"
       markdownToHtml ".iolaus.css" "manual/manual.md" "manual.html"
       addDependencies "manpages" (map snd hs)
       addDependencies "html" ("index.html" : "FAQ.html" : "TODO.html":
                               "manual.html" : map fst hs)
       addDependencies "doc" ["manpages", "html"]
    where mdToHtml md ht title =
              do rule [ht] [md] $
                    do mdin <- cat md
                       markdownStringToHtmlString
                           ".iolaus.css" (prefix "" title++mdin) >>= mkFile ht
          lhs2md "Amend.lhs" = "amend-record.md"
          lhs2md (x0:x) = toLower x0 : tolower (take (length x-4) x) ++ ".md"
          lhs2manmd x = lhs2md x ++ ".man"
          nam x = take (length (lhs2md x)-3) (lhs2md x)
          undr x = map sp2u (nam x)
              where sp2u ' ' = '_'
                    sp2u c = c
          dash x = map sp2u (nam x)
              where sp2u ' ' = '-'
                    sp2u c = c
          cmd x = "iolaus "++nam x
          tolower (x:xs) | toLower x /= x = ' ':toLower x: tolower xs
          tolower (x:xs) = x : tolower xs
          tolower "" = ""
          prefix toroot x =
              "\n<object data='"++toroot++"doc/hydra.svg' align='right' "++
               "type='image/svg+xml' width=265> Image here! </object>\n\n"++
              "\n# "++ x++
              "\n[about]("++toroot++"index.html) | "++
              "[manual]("++toroot++"manual.html) | "++
              "[download](http://github.com/droundy/iolaus/downloads) | "++
              "[TODO]("++toroot++"TODO.html) | "++
              "[FAQ]("++toroot++"FAQ.html)\n\n"
          commandPage lhs =
           do rule ["manual/"++lhs2md lhs, "manual/"++lhs2manmd lhs]
                   ["preproc", "Iolaus/Commands/"++lhs] $
                do hsin <- cat ("Iolaus/Commands/"++lhs)
                   mkFile ("manual/"++lhs2md lhs++".in") $
                           ("\n\n\\haskell{"++undr lhs++"_description}\n\n"++
                            "\\options{"++nam lhs++"}\n\n"++
                            "\\haskell{"++undr lhs++"_help}\n\n"++
                            hsin)
                   x <- systemOut "./preproc" [nam lhs,
                                               "Iolaus/Commands/"++lhs]
                   mkdir "manual"
                   mkdir "man"
                   mkdir "man/man1"
                   let header = "% iolaus-"++nam lhs++"(1)\n"++
                                "% David Roundy\n"++
                                "% date?\n\n"
                   mkFile ("manual/"++lhs2md lhs)
                          (header++prefix "../" (cmd lhs)++x)
                   mkFile ("manual/"++lhs2manmd lhs) (header++x)
              m <-markdownToMan ("manual/"++lhs2manmd lhs)
                                ("man/man1/iolaus-"++dash lhs++".1")
              h <- markdownToHtml "../.iolaus.css"
                   ("manual/"++lhs2md lhs) ("manual/"++dash lhs++".html")
              return (h,m)

allTests =
   do here <- pwd
      rm_rf "tests/tmp"
      rm_rf "tests/network/tmp"
      let onetest _ f | not (".sh" `isSuffixOf` f) = return []
          onetest prefix f =
              do fcontents <- words `fmap` cat f
                 let testFor k = "not-for-"++k `notElem` fcontents
                 alwaysFails <-
                     do amw <- amInWindows
                        return (amw && "fails-on-wine" `elem` fcontents)
                 withDirectory ("tmp/"++f) $
                     do let testname = if "test-fails" `elem` fcontents
                                           || alwaysFails
                                       then "failing-"++prefix++f
                                       else prefix++f
                        testScript testname "shell" ("../../"++f)
                        addToRule testname $
                            do addToPath here
                               mapM_ (uncurry setEnv)
                                         [("EMAIL", "tester")]
                               pwd >>= setEnv "HOME"
                        return [testname]
      networkTests <- concat `fmap`
                      mapDirectory (onetest "network-") "tests/network"
      testSuite "network-test" (sort networkTests)
      alltests <- concat `fmap` mapDirectory (onetest "") "tests"
      let (failing, passing) = partition ("failing-" `isPrefixOf`) alltests
      testSuite "failing-test" (sort failing)
      testSuite "local-test" (sort passing ++ sort failing)
      testSuite "test" ["network-test","local-test"]
