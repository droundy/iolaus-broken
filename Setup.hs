#!/usr/bin/runhaskell
import Distribution.Franchise.V1
import Data.List ( sort, partition, isPrefixOf, isSuffixOf )

main = build [configurableProgram "shell" "bash" ["shsh","sh"],
              flag "with-type-witnesses" "for gadt type witnesses"
                (do putS "compiling with type witnesses enabled"
                    define "GADT_WITNESSES"
                    ghcFlags ["-fglasgow-exts"])] $
       do hcFlags ["-Iinclude"]
          ghcFlags ["-Wall","-threaded"]
          withDirectory "etc" $ etc "bash_completion.d/iolaus"
          withModule "System.Process.Redirects" $ define "HAVE_REDIRECTS"
          executable "iolaus" "iolaus.hs" []
          executable "git-imof" "git-imof.hs" []
          enforceAllPrivacy
          allTests

allTests =
   do here <- pwd
      rm_rf "tests/tmp"
      rm_rf "tests/network/tmp"
      let onetest _ f | not (".sh" `isSuffixOf` f) = return []
          onetest prefix f =
              do fcontents <- words `fmap` cat f
                 let testFor k = "not-for-"++k `notElem` fcontents
                 alwaysFails <- do amw <- amInWindows
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
      networkTests <- concat `fmap` mapDirectory (onetest "network-") "tests/network"
      testSuite "network-test" (sort networkTests)
      alltests <- concat `fmap` mapDirectory (onetest "") "tests"
      let (failing, passing) = partition ("failing-" `isPrefixOf`) alltests
      testSuite "failing-test" (sort failing)
      testSuite "local-test" (sort passing ++ sort failing)
      testSuite "test" ["network-test","local-test"]
