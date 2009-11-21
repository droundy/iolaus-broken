import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(..) )
import Text.Regex ( matchRegex, mkRegex )

import Iolaus.Arguments ( optionDescription )
import Iolaus.Command ( CommandControl(..), Command(..), extract_commands,
                        basic_options, adv_options )
import Iolaus.Commands ( command_control_list )

the_commands :: [Command]
the_commands = extract_commands command_control_list

data Formatting = Html | Plain

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> preproc ["\\input{"++x++"}"]
    ["--html",c,fn] ->
        do putStrLn $ "\n# NAME\n"
           putStr $ "iolaus "++c++" - "
           putStrLn $ command_property command_description the_commands c
           putStrLn "\n# SYNOPSIS"
           putStrLn $ "<div class='verseblock'>"
           putStrLn $ "*iolaus "++c++"*  "++get_synopsis Html c
           putStrLn $ "</div>"
           putStrLn "\n# DESCRIPTION"
           putStrLn $ command_property command_help the_commands c
           cs <- readFile fn
           preproc (lines cs)
           putStrLn "\n# OPTIONS"
           putStrLn $ get_options c
    [c,fn] -> do putStrLn $ "\n# NAME\n"
                 putStr $ "iolaus "++c++" - "
                 putStrLn $ command_property command_description the_commands c
                 putStrLn "\n# SYNOPSIS"
                 putStrLn $ "*iolaus "++c++"*  "++get_synopsis Plain c
                 putStrLn "\n# DESCRIPTION"
                 putStrLn $ command_property command_help the_commands c
                 cs <- readFile fn
                 preproc (lines cs)
                 putStrLn "\n# OPTIONS"
                 putStrLn $ get_options c
    _ -> exitWith $ ExitFailure 1

preproc :: [String] -> IO ()
preproc (('%':_):ss) = preproc ss
preproc ("\\begin{code}":ss) = ignore ss
preproc ("\\begin{options}":ss) =
    do putStr $ unlines $ map ("    "++) os
       preproc $ drop 1 $ dropWhile (/= "\\end{options}") ss
    where os = takeWhile (/= "\\end{options}") ss
preproc ("Table of Contents":ss) =
    do mapM_ putStrLn (map show command_control_list)
       preproc ss
preproc (s:ss) = do
  case matchRegex (mkRegex "^\\\\input\\{(.+)\\}$") s of
    Just (fn:_) -> do cs <- readFile fn
                      preproc $ lines cs
    _ -> case matchRegex (mkRegex "^(.*)\\\\haskell\\{(.+)\\}(.*)$") s of
         Just (before:var:after:_) ->
             case breakLast '_' var of
             (cn,"help") -> putStrLn (before++gh cn++after)
             (cn,"description") -> putStrLn (before++gd cn++after)
             ("iolaus","version") -> putStrLn (before++"OOOPS"++after)
             aack -> error $ show aack
         _ -> case matchRegex (mkRegex "^(.*)\\\\options\\{(.+)\\}(.*)$") s of
              Just (before:comm:after:_) ->
                  putStrLn (before++get_options comm++after)
              _ -> putStrLn s
  preproc ss
  where breakLast chr str = (reverse $ tail l, reverse f)
            where (f, l) = break (==chr) $ reverse str
preproc [] = return ()

get_options :: String -> String
get_options comm = get_com_options $ get_c names the_commands
    where names = words comm

get_synopsis :: Formatting -> String -> String
get_synopsis f comm = unwords $ get_com_synopsis f (get_c names the_commands)++
                      command_extra_arg_help (last $ get_c names the_commands)
    where names = words comm

instance Show CommandControl where
    show (Command_data (Command { command_name = n})) =
              "- [iolaus "++n++"](manual/"++n++".html)"
    show (Command_data (SuperCommand { command_name = n,
                                       command_sub_commands = cs })) =
          "- iolaus "++n++"  \n" ++ init (unlines
           (map (\c ->"    - [iolaus "++n++" "++c++
                      "](manual/"++n++"-"++c++".html)")
            (map command_name $ extract_commands cs)))
    show (Group_name g) = "\n"++g++"\n"
    show (Hidden_command _) = ""

get_c :: [String] -> [Command] -> [Command]
get_c (name:ns) commands =
    case ns of
    [] -> [get name commands]
    _ -> case get name commands of
         c@SuperCommand { } ->
             c:(get_c ns $ extract_commands $ command_sub_commands c)
         _ ->
             error $ "Not a supercommand: " ++ name
    where get "amend" cs = get "amend-record" cs
          get n (c:cs) | command_name c == n = c
                       | otherwise = get n cs
          get n [] = error $ "No such command:  "++n
get_c [] _ = error "no command specified"

get_com_synopsis :: Formatting -> [Command] -> [String]
get_com_synopsis f cs = map bar $ map optionDescription $
                        basic_options (Command_data c)
    where c = last cs
          bar xs = case f of
                     Html -> "<nobr>["++pipethem (map fst xs)++"]</nobr>"
                     Plain -> "["++pipethem (map fst xs)++"]"
          pipethem (x:y:z) = pipeit x ++ " | " ++ pipethem (y:z)
          pipethem [x] = pipeit x
          pipethem [] = ""
          pipeit (',':' ':r) = " | "++pipeit r
          pipeit (x:xs) = x : pipeit xs
          pipeit "" = ""

get_com_options :: [Command] -> String
get_com_options cs = unlines $ map bar $ concatMap optionDescription $
                     (basic_options (Command_data $ last cs) ++
                      adv_options (Command_data $ last cs))
    where bar (x,d) = x++"\n:\t"++chompnewline d
          chompnewline "" = ""
          chompnewline x | last x == '\n' = chompnewline (init x)
                         | otherwise = x

ignore :: [String] -> IO ()
ignore ("\\end{code}":ss) = preproc ss
ignore (_:ss) = ignore ss
ignore [] = return ()

command_property :: (Command -> String) -> [Command] -> String
                 -> String
command_property property commands name =
    property $ last c
    where names = words name
          c = get_c names commands

gh :: String -> String
gh = command_property command_help the_commands
gd :: String -> String
gd c = unlines $ map (\x -> "> *"++x++"*") $ lines $
       command_property command_description the_commands c
