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
          executable "git-imof" "git-imof.hs" []
          enforceAllPrivacy
          doc
          allTests

doc =
    do privateExecutable "preproc" "preproc.hs" []
       xs <- (filter (/= "Show.lhs") .
              filter (".lhs" `isSuffixOf`)) `fmap` ls "Iolaus/Commands"
       hs <- mapM commandPage $ xs
       rule ["manual/manual.md"] ("preproc":"doc/iolaus.md":hs) $
            do x <- systemOut "./preproc" ["doc/iolaus.md"]
               let toc = sort $ zipWith mklink xs hs
                   mklink lhs h = "["++cmd lhs++"]("++h++")  "
               mkFile "manual/manual.md" (unlines $ x:toc)
       markdownToHtml ".iolaus.css" "README.md" "index.html"
       markdownToHtml ".iolaus.css" "manual/manual.md" "manual.html"
       addDependencies "html" ("index.html":"manual.html":hs)
    where lhs2md (x0:x) = toLower x0 : tolower (take (length x-4) x) ++ ".md"
          nam x = take (length (lhs2md x)-3) (lhs2md x)
          undr x = map sp2u (nam x)
              where sp2u ' ' = '_'
                    sp2u c = c
          cmd x = "iolaus "++nam x
          tolower (x:xs) | toLower x /= x = ' ':toLower x: tolower xs
          tolower (x:xs) = x : tolower xs
          tolower "" = ""
          prefix x = "# "++ cmd x++
                     " <img src='../doc/hydra.svg' align='right'>\n\n"
          commandPage lhs =
           do rule ["manual/"++lhs2md lhs]
                   ["preproc", "Iolaus/Commands/"++lhs] $
                do hsin <- cat ("Iolaus/Commands/"++lhs)
                   mkFile ("manual/"++lhs2md lhs++".in") $
                           ("\n\n\\haskell{"++undr lhs++"_description}\n\n"++
                            "\\options{"++nam lhs++"}\n\n"++
                            "\\haskell{"++undr lhs++"_help}\n\n"++
                            hsin)
                   x <- systemOut "./preproc" ["manual/"++lhs2md lhs++".in"]
                   mkdir "manual"
                   mkFile ("manual/"++lhs2md lhs) (prefix lhs++x)
              markdownToHtml "../.iolaus.css" ("manual/"++lhs2md lhs)
                                              ("manual/"++nam lhs++".html")

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
