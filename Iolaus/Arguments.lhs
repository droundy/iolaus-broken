%  Copyright (C) 2002-2004 David Roundy
%
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation; either version 2, or (at your option)
%  any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; see the file COPYING.  If not, write to
%  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
%  Boston, MA 02110-1301, USA.

\begin{code}
{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP #-}

#include "gadts.h"

module Iolaus.Arguments
    ( Flag( .. ), flagToString, optionDescription,
      isin, arein, commitApproach, modifySafely,
      recordDeltaDebug, fixSubPaths, areFileArgs, author,
      dryrun, IolausOption( .. ), option_from_iolausoption, help,
      list_options, pull_apart_option, max_count, help_on_match,
      any_verbosity, disable, notest, test, working_repo_dir,
      testByDefault, remote_repo, possibly_remote_repo_dir, patchname_option,
      logfile, rmlogfile, output, output_auto_name, ask_long_comment,
      sign, verify, edit_description, reponame, apply_conflict_options,
      pull_conflict_options, repo_combinator, only_to_files, changes_format,
      commit_format, match_one_nontag, all_interactive, summary,
      match_one, match_several, match_range, match_several_or_range,
      match_several_or_last, match_several_or_first, config_defaults,
      sibling, flagsToSiblings, files, directories, nullFlag,
      patch_select_flag ) where
import System.Console.GetOpt
import Data.List ( nub )
import Data.Maybe ( fromMaybe, listToMaybe )
import Control.Monad ( unless )
import Data.Char ( isDigit )

import Iolaus.Utils ( withCurrentDirectory )
import Iolaus.RepoPath ( AbsolutePath, AbsolutePathOrStd, SubPath,
                         toFilePath, makeSubPathOf, simpleSubPath,
                         ioAbsolute, makeAbsolute, makeAbsoluteOrStd )
import Iolaus.Flags ( Flag(..) )
#include "impossible.h"

data FlagContent = NoContent | AbsoluteContent AbsolutePath
                 | AbsoluteOrStdContent AbsolutePathOrStd
                 | StringContent String
                   deriving (Eq, Show, Ord)

-- getContent is very tedious to write, but this is the only way (that
-- I know of) to guarantee that it works for all flags (which then
-- guarantees that isAnAbsolute, isa, flagToString, etc also work
-- properly)

-- | 'get_content' returns the content of a flag, if any.
-- For instance, the content of @Author \"Louis Aragon\"@ is @StringContent
-- \"Louis Aragon\"@, while the content of @Pipe@ is @NoContent@
getContent :: Flag -> FlagContent
getContent (PatchName s) = StringContent s
getContent (Output s) = AbsoluteOrStdContent s
getContent NoCauterizeAllHeads = NoContent
getContent CauterizeAllHeads = NoContent
getContent DeltaDebugWorkingSubset = NoContent
getContent ShowMerges = NoContent
getContent HideMerges = NoContent
getContent ShowParents = NoContent
getContent ShowHash = NoContent
getContent NoShowHash = NoContent
getContent ShowTested = NoContent
getContent HideTested = NoContent
getContent Nice = NoContent
getContent NotNice = NoContent
getContent (CommutePast n) = StringContent (show n)
getContent (RecordFor r) = StringContent r
getContent Verbose = NoContent
getContent Help = NoContent
getContent ListOptions = NoContent
getContent Test = NoContent
getContent Build = NoContent
getContent TestParents = NoContent
getContent NoTest = NoContent
getContent HelpOnMatch = NoContent
getContent OnlyChangesToFiles = NoContent
getContent LeaveTestDir = NoContent
getContent NoLeaveTestDir = NoContent
getContent Timings = NoContent
getContent Debug = NoContent
getContent DebugVerbose = NoContent
getContent NormalVerbosity = NoContent
getContent Quiet = NoContent
getContent (Author s) = StringContent s
getContent (OnePatch s) = StringContent s
getContent (SeveralPatch s) = StringContent s
getContent (AfterPatch s) = StringContent s
getContent (UpToPatch s) = StringContent s
getContent (TagName s) = StringContent s
getContent (LastN s) = StringContent (show s)
getContent (MaxC s) = StringContent (show s)
getContent (OneTag s) = StringContent s
getContent (AfterTag s) = StringContent s
getContent (UpToTag s) = StringContent s
getContent (LogFile s) = AbsoluteContent s
getContent (OutputAutoName s) = AbsoluteContent s
getContent (PatchIndexRange _ _) = NoContent -- FIXME this doesn't fit into a neat category
getContent Count = NoContent
getContent All = NoContent
getContent RmLogFile = NoContent
getContent (SignAs s) = StringContent s
getContent (Verify s) = AbsoluteContent s
getContent VerifyAny = NoContent
getContent NonVerify = NoContent
getContent Intersection = NoContent
getContent Unified = NoContent
getContent Union = NoContent
getContent Sign = NoContent
getContent NoSign = NoContent
getContent (WorkDir s) = StringContent s
getContent (RepoDir s) = StringContent s
getContent (RemoteRepo s) = StringContent s
getContent EditDescription = NoContent
getContent NoEditDescription = NoContent
getContent EditLongComment = NoContent
getContent NoEditLongComment = NoContent
getContent PromptLongComment = NoContent
getContent AllowConflicts = NoContent
getContent NoAllowConflicts = NoContent
getContent Interactive = NoContent
getContent Summary = NoContent
getContent NoSummary = NoContent
getContent Reverse = NoContent
getContent Graph = NoContent
getContent (FixFilePath _ _) = NoContent -- FIXME!!!
getContent DryRun = NoContent
getContent GlobalConfig = NoContent
getContent SystemConfig = NoContent
getContent ConfigDefault = NoContent
getContent Disable = NoContent
getContent Files = NoContent
getContent NoFiles = NoContent
getContent Directories = NoContent
getContent NoDirectories = NoContent
getContent (Sibling s) = AbsoluteContent s
getContent NullFlag = NoContent
getContent (UMask s) = StringContent s

get_content :: Flag -> Maybe String
get_content f = do StringContent s <- Just $ getContent f
                   return s

-- | @a `'isa'` b@ tests whether @a@ is flag @b@ with a string argument.
-- @b@ typically is a Flag constructor expecting a string
-- For example, @(Author \"Ted Hughes\") `isa` Author@ returns true.
isa :: Flag -> (String -> Flag) -> Bool
a `isa` b = case get_content a of
            Nothing -> False
            Just s -> a == b s

-- | @a `'isAnAbsolute'` b@ tests whether @a@ is flag @b@ with an absolute path argument.
-- @b@ typically is a Flag constructor expecting an absolute path argument
-- For example, @(Context contextfile) `isAnAbsolute` Context@ returns true.
isAnAbsolute :: Flag -> (AbsolutePath -> Flag) -> Bool
isAnAbsolute f x = case getContent f of
                  AbsoluteContent s -> f == x s
                  _ -> False

-- | @a `'isAnAbsoluteOrStd'` b@ tests whether @a@ is flag @b@ with a path argument.
-- @b@ typically is a Flag constructor expecting a path argument
-- For example, @(Output o) `isAnAbsoluteOrStd` @ returns true.
isAnAbsoluteOrStd :: Flag -> (AbsolutePathOrStd -> Flag) -> Bool
isAnAbsoluteOrStd f x = case getContent f of
                          AbsoluteOrStdContent s -> f == x s
                          _ -> False

isin :: (String->Flag) -> [Flag] -> Bool
f `isin` fs = any (`isa` f) fs

arein :: [IolausOption] -> [Flag] -> Bool
(IolausNoArgOption _ _ f _ : dos') `arein` fs
    = f `elem` fs || dos' `arein` fs
(IolausArgOption _ _ f _ _ : dos') `arein` fs
    = f `isin` fs || dos' `arein` fs
(IolausAbsPathOption _ _ f _ _ : dos') `arein` fs
    = any (`isAnAbsolute` f) fs || dos' `arein` fs
(IolausAbsPathOrStdOption _ _ f _ _ : dos') `arein` fs
    = any (`isAnAbsoluteOrStd` f) fs || dos' `arein` fs
(IolausOptAbsPathOption _ _ _ f _ _ : dos') `arein` fs
    = any (`isAnAbsolute` f) fs || dos' `arein` fs
(IolausMultipleChoiceOption os: dos') `arein` fs
    = os `arein` fs || dos' `arein` fs
[] `arein` _ = False

-- | A type for iolaus' options. The value contains the command line
-- switch(es) for the option, a help string, and a function to build a
-- @Flag@ from the command line arguments.  for each constructor,
-- 'shortSwitches' represents the list of short command line switches
-- which invoke the option, longSwitches the list of long command line
-- switches, optDescr the description of the option, and argDescr the description
-- of its argument, if any. mkFlag is a function which makes a @Flag@ from
-- the arguments of the option.
data IolausOption
    = IolausArgOption [Char] [String] (String->Flag) String String
    -- ^ @IolausArgOption shortSwitches longSwitches mkFlag ArgDescr OptDescr@
    -- The constructor for options with a string argument, such as
    -- @--tag@

    | IolausAbsPathOption [Char] [String] (AbsolutePath -> Flag) String String
    -- ^ @IolausAbsPathOption shortSwitches longSwitches mkFlag ArgDescr OptDescr@
    -- The constructor for options with an absolute path argument, such as
    -- @--sibling@

    | IolausAbsPathOrStdOption [Char] [String] (AbsolutePathOrStd -> Flag) String String
    -- ^ @IolausAbsPathOrStdOption shortSwitches longSwitches mkFlag ArgDescr OptDescr@
    -- The constructor for options with a path argument, such as @-o@

    | IolausOptAbsPathOption [Char] [String] String (AbsolutePath -> Flag) String String
    -- ^ @IolausOptAbsPathOrStdOption shortSwitches longSwitches defaultPath
    -- mkFlag ArgDescr OptDescr@ where defaultPath is a default value
    -- for the Path, as a string to be parsed as if it had been given
    -- on the command line.
    -- The constructor for options with an optional path argument, such as @-O@

    | IolausNoArgOption [Char] [String] Flag String
    -- ^ @IolausNoArgOption shortSwitches longSwitches mkFlag optDescr@
    -- The constructon fon options with no arguments.

    | IolausMultipleChoiceOption [IolausOption]
    -- ^ A constructor for grouping mutually-exclusive options together.

optionDescription :: IolausOption -> [(String,String)]
optionDescription (IolausAbsPathOrStdOption s l _ n h) =
    [(listflags (shortarg n s++longarg n l), h)]
optionDescription (IolausAbsPathOption s l _ n h) =
    [(listflags (shortarg n s++longarg n l), h)]
optionDescription (IolausArgOption s l _ n h) =
    [(listflags (shortarg n s++longarg n l), h)]
optionDescription (IolausNoArgOption s l _ h) =
    [(listflags (short s++long l), h)]
optionDescription (IolausMultipleChoiceOption os) =
    concatMap optionDescription os
optionDescription _ = [("bunnies!","more")]

listflags :: [String] -> String
listflags (a:b:r) = a++", "++listflags (b:r)
listflags [a] = a
listflags [] = ""

longarg :: String -> [String] -> [String]
longarg n = map (\l -> "--"++l++"="++n)

shortarg :: String -> [Char] -> [String]
shortarg n = map (\c -> '-':c:' ':n)

short :: [Char] -> [String]
short = map (\c -> ['-',c])

long :: [String] -> [String]
long = map ("--"++)

instance Eq IolausOption where -- This is a very hokey comparison...
    IolausMultipleChoiceOption xs == IolausMultipleChoiceOption ys
        = xs == ys
    IolausNoArgOption _ x _ _ == IolausNoArgOption _ y _ _ = x == y
    IolausOptAbsPathOption _ x _ _ _ _ == IolausOptAbsPathOption _ y _ _ _ _ =
        x == y
    IolausAbsPathOrStdOption _ x _ _ _ == IolausAbsPathOrStdOption _ y _ _ _ =
        x == y
    IolausAbsPathOption _ x _ _ _ == IolausAbsPathOption _ y _ _ _ = x == y
    IolausArgOption _ x _ _ _ == IolausArgOption _ y _ _ _ = x == y
    _ == _ = False

pull_apart_option :: Flag -> IolausOption -> [Either String (String, String)]
pull_apart_option f (IolausAbsPathOption _ [n] o _ _)
    = case getContent f of
        AbsoluteContent s -> if f == o s then [Right (n, toFilePath s)]
                                         else []
        _ -> []
pull_apart_option f (IolausArgOption _ [n] o _ _)
    = case get_content f of
        Nothing -> []
        Just s -> if f == o s then [Right (n, s)]
                              else []
pull_apart_option f (IolausNoArgOption _ [n] o _)
    | f == o = [Right (n, "true")]
pull_apart_option f (IolausMultipleChoiceOption os)
    | any (not . null . pull_apart_option f) os = concatMap doit os
    where doit o | not $ null $ pull_apart_option f o = pull_apart_option f o
          doit (IolausArgOption _ [n] _ _ _) = [Left n]
          doit (IolausAbsPathOption _ [n] _ _ _) = [Left n]
          doit (IolausNoArgOption _ [n] _ _) = [Left n]
          doit _ = []
pull_apart_option _ _ = []

option_from_iolausoption :: AbsolutePath -> IolausOption -> [OptDescr Flag]
option_from_iolausoption _ (IolausNoArgOption a b c h) = [Option a b (NoArg c) h]
option_from_iolausoption _ (IolausArgOption a b c n h) = [Option a b (ReqArg c n) h]
option_from_iolausoption wd (IolausMultipleChoiceOption os) = concatMap (option_from_iolausoption wd) os
option_from_iolausoption wd (IolausAbsPathOrStdOption a b c n h) = [Option a b (ReqArg (c . makeAbsoluteOrStd wd) n) h]
option_from_iolausoption wd (IolausAbsPathOption a b c n h) = [Option a b (ReqArg (c . makeAbsolute wd) n) h]
option_from_iolausoption wd (IolausOptAbsPathOption a b d c n h) = [Option a b (OptArg (c . makeAbsolute wd . fromMaybe d) n) h]

-- | 'concat_option' creates a IolausMultipleChoiceOption from a list of
-- option, flattening any IolausMultipleChoiceOption in the list.
concat_options :: [IolausOption] -> IolausOption
concat_options os = IolausMultipleChoiceOption $ concatMap from_option os
 where
  from_option (IolausMultipleChoiceOption xs) = xs
  from_option x = [x]

extract_fix_path :: [Flag] -> Maybe (AbsolutePath, AbsolutePath)
extract_fix_path [] = Nothing
extract_fix_path ((FixFilePath repo orig):_)  = Just (repo, orig)
extract_fix_path (_:fs) = extract_fix_path fs

fixSubPaths :: [Flag] -> [FilePath] -> IO [SubPath]
fixSubPaths flags fs =
    withCurrentDirectory o $
    do fixedfs <- mapM fixit $ filter (not.null) fs
       let (good, bad) = partitionEither fixedfs
       unless (null bad) $
              putStrLn $ "Ignoring non-repository paths: " ++ unwords bad
       return $ nub good
 where
    (r,o) = case extract_fix_path flags of
            Just xxx -> xxx
            Nothing -> bug "Can't fix path in fixSubPaths"
    fixit p = do ap <- ioAbsolute p
                 case makeSubPathOf r ap of
                   Just sp -> return $ Right sp
                   Nothing -> return $ maybe (Left p) Right $ simpleSubPath p

partitionEither :: [Either a b] -> ([b],[a])
partitionEither es = ( [b | Right b <- es]
                     , [a | Left  a <- es] )

-- as opposed to just '.'
areFileArgs :: [SubPath] -> Bool
areFileArgs rps = concatMap toFilePath rps /= ""

-- | 'list_option' is an option which lists the command's arguments
list_options :: IolausOption
list_options = IolausNoArgOption [] ["list-options"] ListOptions
               "simply list the command's arguments"

flagToString :: [IolausOption] -> Flag -> Maybe String
flagToString x f = listToMaybe $ concatMap f2o x
    where f2o o = case pull_apart_option f o of
                    Left s:_ -> [s]
                    Right (s1,"true"):_ -> ["--"++s1]
                    Right (s1,s2):_ -> ["--"++s1++"="++show s2]
                    [] -> []

working_repo_dir :: IolausOption
possibly_remote_repo_dir :: IolausOption
disable :: IolausOption

all_interactive, all_patches, interactive, ask_long_comment, match_one_nontag,
  pull_conflict_options, apply_conflict_options,
  patchname_option, edit_description, output, output_auto_name, repo_combinator,
  summary, match_several_or_range, match_several_or_last,
  match_several_or_first, help, help_on_match,
  match_one, match_range, match_several, logfile, rmlogfile :: IolausOption
\end{code}

\section{Common options to iolaus commands}

\begin{options}
--help
\end{options}
Every \verb|COMMAND| accepts \verb!--help! as an argument, which tells it to
provide a bit of help.  Among other things, this help always provides an
accurate listing of the options available with that command, and is
guaranteed never to be out of sync with the version of iolaus you actually
have installed (unlike this manual, which could be for an entirely
different version of iolaus).
\begin{verbatim}
% iolaus COMMAND --help
\end{verbatim}
\begin{code}
help = IolausNoArgOption ['h'] ["help"] Help
       "shows brief description of command and its arguments"

help_on_match = IolausNoArgOption [] ["match"] HelpOnMatch
       "shows a summary of how to use patch matching rules"
\end{code}

\begin{options}
--disable
\end{options}

Every `COMMAND` accepts the `--disable` option, which can be used
with `iolaus COMMAND --config-default --disable` to disable some commands in
the repository. This can be helpful if you want to protect the
repository from accidental use of
advanced commands like unrecord or amend-record.

\begin{code}
disable = IolausNoArgOption [] ["disable"] Disable
        "disable this command"
\end{code}

\begin{options}
--verbose, --quiet, --normal-verbosity
\end{options}
Most commands also accept the \verb!--verbose! option, which tells iolaus to
provide additional output.  The amount of verbosity varies from command to
command.  Commands that accept \verb!--verbose\verb! also accept \verb!--quiet\verb!,
which surpresses non-error output, and \verb!--normal-verbosity\verb! which can be
used to restore the default verbosity if \verb!--verbose! or \verb!--quiet! is in
the defaults file.

\begin{options}
--debug
\end{options}
Many commands also accept the \verb!--debug! option, which causes iolaus to generate
additional output that may be useful for debugging its behavior, but which otherwise
would not be interesting.
\begin{code}
any_verbosity :: [IolausOption]
any_verbosity =[IolausMultipleChoiceOption
                [IolausNoArgOption [] ["debug"] Debug
                 "give only debug output",
                 IolausNoArgOption [] ["debug-verbose"] DebugVerbose
                 "give debug and verbose output",
                 IolausNoArgOption ['v'] ["verbose"] Verbose
                 "give verbose output",
                 IolausNoArgOption ['q'] ["quiet"] Quiet
                 "suppress informational output",
                 IolausNoArgOption [] ["standard-verbosity"] NormalVerbosity
                 "neither verbose nor quiet output"],
                 IolausNoArgOption [] ["timings"] Timings "provide debugging timings information"]
\end{code}

\begin{options}
--repodir
\end{options}
Another common option is the \verb!--repodir! option, which allows you to
specify the directory of the repository in which to perform the command.
This option is used with commands, such as whatsnew, that ordinarily would
be performed within a repository directory, and allows you to use those
commands without actually being in the repository directory when calling the
command.  This is useful when running iolaus in a pipe, as might be the case
when running \verb'apply' from a mailer.

\begin{code}
working_repo_dir = IolausArgOption [] ["repodir"] WorkDir "DIRECTORY"
             "specify the repository directory in which to run"
possibly_remote_repo_dir = IolausArgOption [] ["repo"] RepoDir "URL"
             "specify the repository URL"
\end{code}

\begin{options}
--remote-repo
\end{options}

Some commands, such as \verb'pull' require a remote repository to be specified,
either from the command line or as a default.  The \verb!--remote-repo!
provides an alternative way to supply this remote repository path.  This flag
can be seen as temporarily ``replacing'' the default repository. Setting it
causes the command to ignore the default repository (it also does not affect,
i.e. overwrite the default repository).  On the other hand, if any other
repositories are supplied as command line arguments, this flag will be ignored
(and the default repository may be overwritten).

\begin{code}
-- | 'remote_repo' is the option used to specify the URL of the remote
-- repository to work with
remote_repo :: IolausOption
remote_repo = IolausArgOption [] ["remote-repo"] RemoteRepo "URL"
             "specify the remote repository URL to work with"
\end{code}

\input{Iolaus/Match.lhs}
\input{Iolaus/PatchMatch.lhs}

\begin{code}
author :: IolausOption
author = IolausArgOption ['A'] ["author"] Author "EMAIL" "specify author id"

patchname_option = IolausArgOption ['m'] ["patch-name"] PatchName "PATCHNAME"
                   "name of patch"

match_one = concat_options [__patch, __tag, __index]
match_one_nontag = concat_options [__patch, __index]
match_several    = concat_options [__patches, __tags]
match_range            = concat_options [match_to, match_from, __patch, __last, __indexes]
match_several_or_range = concat_options [match_to, match_from, __last, __indexes,
                                         __patches, __tags]
match_several_or_first = concat_options [match_to, __indexes, __patches, __tags]
match_several_or_last  = concat_options [match_from, __last, __patches, __tags]

match_to, match_from :: IolausOption
match_to = IolausMultipleChoiceOption
            [IolausArgOption [] ["to-patch"] UpToPatch "REGEXP"
             "select changes up to a patch matching REGEXP",
             IolausArgOption [] ["to-tag"] UpToTag "REGEXP"
             "select changes up to a tag matching REGEXP"]
match_from = IolausMultipleChoiceOption
              [IolausArgOption [] ["from-patch"] AfterPatch "REGEXP"
               "select changes starting with a patch matching REGEXP",
               IolausArgOption [] ["from-tag"] AfterTag "REGEXP"
               "select changes starting with a tag matching REGEXP"]

__tag, __tags, __patch, __patches, __last, __index, __indexes :: IolausOption

__tag = IolausArgOption ['t'] ["tag"] OneTag "REGEXP"
       "select tag matching REGEXP"
__tags = IolausArgOption ['t'] ["tags"] OneTag "REGEXP"
        "select tags matching REGEXP"

__patch = IolausArgOption ['p'] ["patch"] OnePatch "REGEXP"
         "select a single patch matching REGEXP"
__patches = IolausArgOption ['p'] ["patches"] SeveralPatch "REGEXP"
           "select patches matching REGEXP"

__last = IolausArgOption [] ["last"] lastn "NUMBER"
         "select the last NUMBER patches"
    where lastn s = if and (map isDigit s)
                    then LastN (read s)
                    else LastN (-1)

max_count :: IolausOption
max_count = IolausArgOption [] ["max-count"] maxc "NUMBER"
         "limit the number of patches displayed"
    where maxc s = if and (map isDigit s)
                   then MaxC (read s)
                   else MaxC (-1)

__index = IolausArgOption ['n'] ["index"] indexrange "N-M" "select a range of patches"
    where indexrange s = if all isDigit s
                         then PatchIndexRange (read s) (read s)
                         else PatchIndexRange 0 0

__indexes = IolausArgOption ['n'] ["index"] indexrange "N-M" "select a range of patches"
    where indexrange s = if all isokay s
                         then if '-' `elem` s
                              then let x1 = takeWhile (/= '-') s
                                       x2 = reverse $ takeWhile (/= '-') $ reverse s
                                   in PatchIndexRange (read x1) (read x2)
                              else PatchIndexRange (read s) (read s)
                         else PatchIndexRange 0 0
          isokay c = isDigit c || c == '-'
\end{code}

\begin{options}
--test, --build, --no-test
\end{options}

If you like, you can configure your repository to be able to run a
test suite of some sort.  You can do this by creating an executable
file named `.test`.  You may also wish to create an executable called
`.build`, which will be run before the test.

\begin{options}
--leave-test-directory, --remove-test-directory
\end{options}

Normally iolaus deletes the directory in which the test was run afterwards.
Sometimes (especially when the test fails) you'd prefer to be able to be
able to examine the test directory after the test is run.  You can do this
by specifying the \verb!--leave-test-directory! flag.  Alas, there is no
way to make iolaus leave the test directory only if the test fails.  The
opposite of \verb!--leave-test-directory! is
\verb!--remove-test-directory!, which could come in handy if you choose to
make \verb!--leave-test-directory! the default (see
section~\ref{defaults}).

\begin{code}
test, notest :: [IolausOption]
notest = [IolausMultipleChoiceOption
          [IolausNoArgOption [] ["no-test"] NoTest "don't run the test script",
           IolausNoArgOption [] ["test"] Test "run the test script",
           IolausNoArgOption [] ["build"] Build "only run the build script",
           IolausNoArgOption [] ["test-parents"]
               TestParents "run the test script on all possibilities"],
          leave_test_dir, nice_test]
test = [IolausMultipleChoiceOption
        [IolausNoArgOption [] ["test"] Test "run the test script",
         IolausNoArgOption [] ["build"] Build "only run the build script",
         IolausNoArgOption [] ["no-test"] NoTest "don't run the test script"],
        leave_test_dir, nice_test]
nice_test :: IolausOption
nice_test = IolausMultipleChoiceOption
            [IolausNoArgOption [] ["nice"] Nice "run test nicely",
             IolausNoArgOption [] ["not-nice"] NotNice "don't run test nicely"]
leave_test_dir :: IolausOption
leave_test_dir = IolausMultipleChoiceOption
                 [IolausNoArgOption [] ["leave-test-directory"]
                  LeaveTestDir "don't remove the test directory",
                  IolausNoArgOption [] ["remove-test-directory"]
                  NoLeaveTestDir "remove the test directory"]

testByDefault :: [Flag] -> [Flag]
testByDefault o = if NoTest `elem` o || Build `elem` o then o
                                                       else Test:o

ask_long_comment =
    IolausMultipleChoiceOption
    [IolausNoArgOption [] ["edit-long-comment"] EditLongComment
     "edit the long comment by default",
     IolausNoArgOption [] ["skip-long-comment"] NoEditLongComment
     "don't give a long comment",
     IolausNoArgOption [] ["prompt-long-comment"] PromptLongComment
     "prompt for whether to edit the long comment"]

logfile = IolausAbsPathOption [] ["logfile"] LogFile "FILE"
          "give patch name and comment in file"

rmlogfile = IolausNoArgOption [] ["delete-logfile"] RmLogFile
            "delete the logfile when done"

summary = IolausMultipleChoiceOption
          [IolausNoArgOption ['s'] ["summary"] Summary "summarize changes",
           IolausNoArgOption [] ["no-summary"] NoSummary "don't summarize changes"]

output = IolausAbsPathOrStdOption ['o'] ["output"] Output "FILE"
         "specify output filename"

output_auto_name = IolausOptAbsPathOption ['O'] ["output-auto-name"] "."
                   OutputAutoName "DIRECTORY"
  "output to automatically named file in DIRECTORY, default: current directory"

edit_description =
    IolausMultipleChoiceOption
    [IolausNoArgOption [] ["edit-description"] EditDescription
                          "edit the patch bundle description",
     IolausNoArgOption [] ["dont-edit-description"] NoEditDescription
                      "don't edit the patch bundle description"]

sign :: IolausOption
sign = IolausMultipleChoiceOption
       [IolausNoArgOption [] ["sign"] Sign
        "sign the patch with your gpg key",
        IolausArgOption [] ["sign-as"] SignAs "KEYID"
        "sign the patch with a given keyid",
        IolausNoArgOption [] ["dont-sign"] NoSign
        "don't sign the patch"]

config_defaults :: [IolausOption]
config_defaults = [IolausNoArgOption [] ["config-default"] ConfigDefault
               "configure the defaults for this repository",
               IolausNoArgOption [] ["global"] GlobalConfig
               "configure the per-user defaults",
               IolausNoArgOption [] ["system"] SystemConfig
               "configure the system-wide defaults"]

verify :: [IolausOption]
verify = [IolausMultipleChoiceOption
          [IolausAbsPathOption [] ["verify-with"] Verify "PUBRING"
           "verify that the patch was signed by a key in PUBRING",
           IolausNoArgOption [] ["verify"] VerifyAny
           "verify that the patch was signed by some key",
           IolausNoArgOption [] ["no-verify"] NonVerify
           "don't verify patch signature"]]

reponame :: IolausOption
reponame = IolausArgOption [] ["repo-name"] RepoDir "DIRECTORY"
           "path of output directory"

commitApproach :: IolausOption
commitApproach = IolausMultipleChoiceOption 
    [IolausNoArgOption [] ["cauterize-all"] CauterizeAllHeads
     "new commit depends on all existing commits",
     IolausArgOption [] ["commute-past"] cp "NUMBER"
     "try commuting past the last NUMBER commits",
     IolausArgOption [] ["record-for"] RecordFor "REPOSITORY"
     "try not to depend on commits not present in REPOSITORY",
     IolausNoArgOption [] ["no-cauterize-all"] NoCauterizeAllHeads
     "commit in minimal context [default]"]
  where cp s = if all isDigit s
               then CommutePast (read s)
               else CommutePast (-1)

modifySafely :: IolausOption
modifySafely = IolausArgOption [] ["record-for"] RecordFor "REPOSITORY"
               "refuse to modify commits present in REPOSITORY"

recordDeltaDebug :: [IolausOption]
recordDeltaDebug =
    [IolausNoArgOption [] ["delta-debug"] DeltaDebugWorkingSubset
     "use delta debugging to avoid recording bugs"]

apply_conflict_options
    = IolausMultipleChoiceOption
      [IolausNoArgOption [] ["allow-conflicts"]
       AllowConflicts "allow conflicts, but don't mark them",
       IolausNoArgOption [] ["no-resolve-conflicts"] NoAllowConflicts
       "equivalent to --dont-allow-conflicts, for backwards compatibility",
       IolausNoArgOption [] ["dont-allow-conflicts"]
       NoAllowConflicts "fail on patches that create conflicts [DEFAULT]"]
pull_conflict_options
    = IolausMultipleChoiceOption
      [IolausNoArgOption [] ["allow-conflicts"]
       AllowConflicts "allow conflicts, but don't mark them",
       IolausNoArgOption [] ["dont-allow-conflicts"]
       NoAllowConflicts "fail on patches that create conflicts"]
\end{code}

\begin{options}
--dry-run
\end{options}
The \verb!--dry-run! option will cause iolaus not to actually take the specified
action, but only print what would have happened.  Not all commands accept
\verb!--dry-run!, but those that do should accept the \verb!--summary!  option.
\begin{code}

dryrun :: String -> [IolausOption]
dryrun v =
    IolausMultipleChoiceOption
    [IolausNoArgOption [] ["dry-run"] DryRun
     ("don't actually "++v++", just show what we would "++v),
     IolausNoArgOption [] ["no-dry-run"] DryRun ("do actually "++v)]
  : IolausMultipleChoiceOption
    [IolausNoArgOption [] ["show-merges"] ShowMerges "show merge commits",
     IolausNoArgOption [] ["hide-merges"] HideMerges "hide merge commits"]
  : commit_format

\end{code}
\begin{options}
--summary, --no-summary
\end{options}
The \verb!--summary! option shows a summary of the patches that would have been
pulled/pushed/whatever.

\begin{code}
commit_format :: [IolausOption]
commit_format =
    [IolausMultipleChoiceOption
     [IolausNoArgOption [] ["show-hash"] ShowHash "show commit hash",
      IolausNoArgOption [] ["no-show-hash"] NoShowHash
                            "don't show commit hash"],
     IolausMultipleChoiceOption
     [IolausNoArgOption [] ["show-tested"] ShowTested "show Tested-On note",
      IolausNoArgOption [] ["hide-tested"] HideTested "hide Tested-On note"],
     IolausMultipleChoiceOption
     [IolausNoArgOption ['s'] ["summary"] Summary "summarize commit changes",
      IolausNoArgOption ['u'] ["show-patch"] Verbose "show commit changes"],
     IolausNoArgOption [] ["show-parents"] ShowParents "show commit parents"]

changes_format :: [IolausOption]
changes_format =
    [IolausNoArgOption [] ["count"] Count "output count of changes",
     IolausNoArgOption [] ["reverse"] Reverse "show changes in reverse order",
     IolausNoArgOption [] ["graph"] Graph "show changes with nice graph",
     IolausMultipleChoiceOption
     [IolausNoArgOption [] ["show-merges"] ShowMerges "show merge commits",
      IolausNoArgOption [] ["hide-merges"] HideMerges "hide merge commits"]]

only_to_files :: IolausOption
only_to_files = IolausNoArgOption [] ["only-to-files"] OnlyChangesToFiles
                "show only changes to specified files"

interactive =
    IolausNoArgOption ['i'] ["interactive"] Interactive
                         "prompt user interactively"
all_patches = IolausNoArgOption ['a'] ["all"] All "answer yes to all patches"

all_interactive = IolausMultipleChoiceOption [all_patches, interactive]

repo_combinator =
    IolausMultipleChoiceOption
    [IolausNoArgOption [] ["intersection"] Intersection
     "take intersection of all repositories",
     IolausNoArgOption [] ["union"] Union
     "take union of all repositories [DEFAULT]"]

sibling :: IolausOption
sibling = IolausAbsPathOption [] ["sibling"] Sibling "URL"
          "specify a sibling directory"

-- | 'flagsToSiblings' collects the contents of all @Sibling@ flags in
-- a list of flags.
flagsToSiblings :: [Flag] -> [AbsolutePath]
flagsToSiblings ((Sibling s) : l) = s : (flagsToSiblings l)
flagsToSiblings (_ : l) = flagsToSiblings l
flagsToSiblings [] = []

files :: IolausOption
files = IolausMultipleChoiceOption
        [IolausNoArgOption [] ["files"] Files
         "include files in output [DEFAULT]",
         IolausNoArgOption [] ["no-files"] NoFiles
         "don't include files in output"]

directories :: IolausOption
directories = IolausMultipleChoiceOption
              [IolausNoArgOption [] ["directories"] Directories
               "include directories in output [DEFAULT]",
               IolausNoArgOption [] ["no-directories"] NoDirectories
               "don't include directories in output"]

nullFlag :: IolausOption        -- "null" is already taken
nullFlag = IolausNoArgOption ['0'] ["null"] NullFlag
       "separate file names by NUL characters"

-- | @'patch_select_flag' f@ holds whenever @f@ is a way of selecting
-- patches such as @PatchName n@.
patch_select_flag :: Flag -> Bool
patch_select_flag All = True
patch_select_flag (PatchName _) = True
patch_select_flag (OnePatch _) = True
patch_select_flag (SeveralPatch _) = True
patch_select_flag (AfterPatch _) = True
patch_select_flag (UpToPatch _) = True
patch_select_flag (TagName _) = True
patch_select_flag (LastN _) = True
patch_select_flag (OneTag _) = True
patch_select_flag (AfterTag _) = True
patch_select_flag (UpToTag _) = True
patch_select_flag _ = False
\end{code}
