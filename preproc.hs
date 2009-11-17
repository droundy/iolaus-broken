import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(..) )
import Text.Regex ( matchRegex, mkRegex )

import Iolaus.Command
    ( Command(SuperCommand), command_sub_commands, command_name,
      command_help, command_description, extract_commands,
      get_command_options_help )
import Iolaus.Help ( command_control_list )

the_commands :: [Command]
the_commands = extract_commands command_control_list

main :: IO ()
main = do
  args <- getArgs
  if length args < 1
     then exitWith $ ExitFailure 1
     else return ()
  c <- preproc ["\\input{"++head args++"}"]
  mapM_ putStrLn c

preproc :: [String] -> IO [String]
preproc (('%':_):ss) = preproc ss
preproc ("\\usepackage{html}":ss) = -- only use html package with latex2html
    do rest <- preproc ss
       return $ "\\usepackage{hyperref}" : rest
preproc ("\\begin{code}":ss) = ignore ss
preproc ("\\begin{options}":ss) =
    do rest <- preproc $ drop 1 $ dropWhile (/= "\\end{options}") ss
       return $ map ("    "++) os ++ rest
    where os = takeWhile (/= "\\end{options}") ss
preproc (s:ss) = do
  rest <- preproc ss
  case matchRegex (mkRegex "^\\\\input\\{(.+)\\}$") s of
    Just (fn:_) -> do cs <- readFile fn -- ratify readFile: not part of
                                        -- darcs executable
                      this <- preproc $ lines cs
                      return $ this ++ rest
    _ -> case matchRegex (mkRegex "^(.*)\\\\haskell\\{(.+)\\}(.*)$") s of
         Just (before:var:after:_) ->
             case breakLast '_' var of
             (cn,"help") -> return $ (before++gh cn++after):rest
             (cn,"description") -> return $ (before++gd cn++after):rest
             ("darcs","version") -> return $ (before++"OOOPS"++after):rest
             aack -> error $ show aack
         _ -> case matchRegex (mkRegex "^(.*)\\\\options\\{(.+)\\}(.*)$") s of
              Just (before:comm:after:_) ->
                  return $ (before++get_options comm++after):rest
              _ ->  case matchRegex (mkRegex "^(.*)\\\\example\\{(.+)\\}(.*)$") s of
                    Just (before:fn:after:_) -> do
                        filecont <- readFile fn -- ratify readFile: not part of
                                                -- darcs executable
                        return $ (before++"\\begin{verbatim}"++
                                  filecont++"\\end{verbatim}"
                                  ++after):rest
                    _ -> return $ s : rest
  where breakLast chr str = (reverse $ tail l, reverse f)
            where (f, l) = break (==chr) $ reverse str

preproc [] = return []

get_options :: String -> String
get_options comm = get_com_options $ get_c names the_commands
    where names = words comm

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

get_com_options :: [Command] -> String
get_com_options [s,c] =
    unlines $ map ("    "++) $ lines $ get_command_options_help (Just s) c
get_com_options [c] =
    unlines $ map ("    "++) $ lines $ get_command_options_help Nothing c

ignore :: [String] -> IO [String]
ignore ("\\end{code}":ss) = preproc ss
ignore (_:ss) = ignore ss
ignore [] = return []

command_property :: (Command -> String) -> [Command] -> String
                 -> String
command_property property commands name =
    property $ last c
    where words_ :: String -> [String] -- "word" with '_' instead of spaces
          words_ s =
              case dropWhile (=='_') s of
                       "" -> []
                       s' -> w : words_ s''
                           where (w, s'') = break (=='_') s'
          names = words_ name
          c = get_c names commands

gh :: String -> String
gh = command_property command_help the_commands
gd :: String -> String
gd c = unlines $ map (\x -> "> *"++x++"*") $ lines $
       command_property command_description the_commands c
