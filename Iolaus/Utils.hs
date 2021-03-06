-- Various utility functions that do not belong anywhere else.

{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Iolaus.Utils ( catchall, ortryrunning, nubsort,
                      clarify_errors, withCurrentDirectory, askUser, stripCr,
                      add_to_error_loc,
                      maybeGetEnv, firstNotBlank, firstJustIO,
                      get_viewer, edit_file, promptYorn, promptCharFancy,
                      without_buffering, formatPath ) where

import Prelude hiding ( catch )
import Control.Exception ( bracket, catch, Exception(IOException),
                           throwIO, try, throw, ioErrors )
import Control.Concurrent ( newEmptyMVar, takeMVar, putMVar, forkIO )
#if !defined(WIN32) ||  __GLASGOW_HASKELL__>=609
import Control.Concurrent ( threadWaitRead )
#endif
import GHC.IOBase ( IOException(ioe_location) )
import System.IO.Error ( isEOFError )

import Iolaus.SignalHandler ( catchNonSignal )
import System.Exit ( ExitCode(..) )
import System.Environment ( getEnv )
import System.IO ( hFlush, stdout, stdin, BufferMode ( NoBuffering ),
                   hLookAhead, hReady, hSetBuffering, hGetBuffering,
                   hIsTerminalDevice )
import Data.Char ( toUpper )
import Iolaus.RepoPath ( FilePathLike, getCurrentDirectory,
                         setCurrentDirectory, toFilePath )
import Data.Maybe ( listToMaybe, isJust )
import Data.List ( group, sort )
import Control.Monad ( when )
import Iolaus.Exec ( exec_interactive )
import Iolaus.Global ( debugMessage )
import Iolaus.Show ( pretty )

#ifdef HAVE_HASKELINE
import System.Console.Haskeline ( runInputT, defaultSettings, getInputLine )
#endif

#ifdef WIN32
import System.Posix.Internals ( getEcho, setCooked, setEcho )
#endif

add_to_error_loc :: Exception -> String -> Exception
add_to_error_loc (IOException ioe) s
    = IOException $ ioe { ioe_location = s ++ ": " ++ ioe_location ioe }
add_to_error_loc e _ = e

catchall :: IO a -> IO a -> IO a
a `catchall` b = a `catchNonSignal` (\_ -> b)

maybeGetEnv :: String -> IO (Maybe String)
maybeGetEnv s = (getEnv s >>= return.Just)
                `catchall` return Nothing -- err can only be isDoesNotExist

firstJustIO :: [IO (Maybe a)] -> IO (Maybe a)
firstJustIO = firstJustM . map (\o -> o `catchall` return Nothing)
    where firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
          firstJustM [] = return Nothing
          firstJustM (e:es) =
              e >>= (\v -> if isJust v then return v else firstJustM es)

clarify_errors :: IO a -> String -> IO a
clarify_errors a e = a `catch` (\x -> fail $ unlines [pretty x,e])

ortryrunning :: IO ExitCode -> IO ExitCode -> IO ExitCode
a `ortryrunning` b = do ret <- try a
                        case ret of
                          (Right ExitSuccess) -> return ExitSuccess
                          _ -> b

withCurrentDirectory :: FilePathLike p => p -> IO a -> IO a
withCurrentDirectory name m =
    bracket
        (do cwd <- getCurrentDirectory
            when (toFilePath name /= "") (setCurrentDirectory name)
            return cwd)
        (\oldwd -> setCurrentDirectory oldwd `catchall` return ())
        (const m)

-- withThread is used to allow ctrl-C to work even while we're waiting for
-- user input.  The job is run in a separate thread, and any exceptions it
-- produces are re-thrown in the parent thread.
withThread :: IO a -> IO a
withThread j = do m <- newEmptyMVar
                  forkIO (runJob m)
                  takeMVar m >>= either throwIO return
    where runJob m = (j >>= putMVar m . Right) `catch` (putMVar m . Left)

askUser :: String -> IO String
#ifdef HAVE_HASKELINE
askUser prompt = runInputT defaultSettings (getInputLine prompt)
                 >>= maybe (error "askUser: unexpected end of input") return
                                      
#else
askUser prompt = withThread $ do putStr prompt
                                 hFlush stdout
                                 waitForStdin
#ifndef WIN32
                                 getLine
#else
                                 stripCr `fmap` getLine
#endif
#endif

waitForStdin :: IO ()
#ifdef WIN32
#if __GLASGOW_HASKELL__ >= 609
waitForStdin = threadWaitRead 0
#else
waitForStdin = return ()  -- threadWaitRead didn't work prior to 6.9
#endif
#else
waitForStdin = threadWaitRead 0
#endif

stripCr :: String -> String
stripCr ""     = ""
stripCr "\r"   = ""
stripCr (c:cs) = c : stripCr cs

-- |Returns Just l where l is first non-blank string in input array; Nothing if no non-blank entries
firstNotBlank :: [String] -> Maybe String
firstNotBlank = listToMaybe . filter (not . null)


-- Format a path for screen output,
-- so that the user sees where the path begins and ends.
-- Could (should?) also warn about unprintable characters here.
formatPath :: String -> String
formatPath path = "\"" ++ quote path ++ "\""
    where quote "" = ""
          quote (c:cs) = if c=='\\' || c=='"'
                         then '\\':c:quote cs
                         else c:quote cs

nubsort :: Ord a => [a] -> [a]
nubsort = map head . group . sort


edit_file :: FilePathLike p => p -> IO ExitCode
edit_file ff = do
  let f = toFilePath ff
  ed <- get_editor
  debugMessage $ "About to execute "++ed++" "++f
  exec_interactive ed f
             `ortryrunning` exec_interactive "emacs" f
             `ortryrunning` exec_interactive "emacs -nw" f
             `ortryrunning` exec_interactive "nano" f
#ifdef WIN32
             `ortryrunning` exec_interactive "edit" f
#endif
get_editor :: IO String
get_editor = getEnv "DARCS_EDITOR" `catchall`
             getEnv "DARCSEDITOR" `catchall`
             getEnv "VISUAL" `catchall`
             getEnv "EDITOR" `catchall` return "vi"

get_viewer :: IO String
get_viewer = getEnv "DARCS_PAGER" `catchall`
             getEnv "PAGER" `catchall` return "less"

promptYorn :: [Char] -> IO Char
promptYorn p = promptCharFancy p "yn" Nothing []

promptCharFancy :: String -> [Char] -> Maybe Char -> [Char] -> IO Char
promptCharFancy p chs md help_chs =
 do a <- withThread $ without_buffering $
           do putStr $ p ++ " ["++ setDefault chs ++"]" ++ helpStr
              hFlush stdout
              waitForStdin
              c <- getChar
#ifdef WIN32
              -- We need to simulate echo
              e <- get_raw_mode
              when e $ putChar c
#endif
              return c
    when (a /= '\n') $ putStr "\n" 
    case () of 
     _ | a `elem` chs                   -> return a
       | a == ' ' -> case md of Nothing -> tryAgain 
                                Just d  -> return d
       | a `elem` help_chs              -> return a
       | otherwise                      -> tryAgain
 where 
 helpStr = case help_chs of
           []    -> ""
           (h:_) -> ", or " ++ (h:" for help: ")
 tryAgain = do putStrLn "Invalid response, try again!"
               promptCharFancy p chs md help_chs
 setDefault s = case md of Nothing -> s
                           Just d  -> map (setUpper d) s
 setUpper d c = if d == c then toUpper c else c

without_buffering :: IO a -> IO a
without_buffering job = bracket nobuf rebuf $ \_ -> job
    where nobuf = do is_term <- hIsTerminalDevice stdin
                     bi <- hGetBuffering stdin
                     raw <- get_raw_mode
                     when is_term $ do hSetBuffering stdin NoBuffering `catch` \_ -> return ()
                                       set_raw_mode True
                     return (bi,raw)
          rebuf (bi,raw) = do is_term <- hIsTerminalDevice stdin
#if SYS == windows
                              buffers <- hGetBuffering stdin
                              hSetBuffering stdin NoBuffering `catch` \_ -> return ()
                              drop_returns
                              hSetBuffering stdin buffers `catch` \_ -> return ()
#else
                              drop_returns
#endif
                              when is_term $ do hSetBuffering stdin bi `catch` \_ -> return ()
                                                set_raw_mode raw
          drop_returns = do is_ready <- hReady stdin `catch` \ e ->
                                        case ioErrors e of
                                          Just x -> if isEOFError x
                                                      then return True
                                                      else throw e
                                          _ -> throw e
                            when is_ready $
                              do waitForStdin
                                 c <- hLookAhead stdin `catch` \_ -> return ' '
                                 when (c == '\n') $
                                   do getChar
                                      drop_returns

-- Code which was in the module RawMode before. Moved here to break cyclic imports
#ifdef WIN32

get_raw_mode :: IO Bool
get_raw_mode = not `fmap` getEcho 0
  `catchall` return False -- getEcho sometimes fails when called from scripts

set_raw_mode :: Bool -> IO ()
set_raw_mode raw = (setCooked 0 normal >> setEcho 0 normal)
   `catchall` return () -- setCooked sometimes fails when called from scripts
 where normal = not raw

#else

get_raw_mode :: IO Bool
get_raw_mode = return False

set_raw_mode :: Bool -> IO ()
set_raw_mode _ = return ()

#endif
