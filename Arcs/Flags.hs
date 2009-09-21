-- Copyright (C) 2002-2004 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

module Arcs.Flags ( ArcsFlag( .. ), Compression( .. ), compression, want_external_merge, isInteractive) where
import Arcs.RepoPath ( AbsolutePath, AbsolutePathOrStd )

-- | The 'ArcsFlag' type is a list of all flags that can ever be
-- passed to darcs, or to one of its commands.
data ArcsFlag = Help | ListOptions | NoTest | Test
               | NoTagOnTest | TagOnTest
               | HelpOnMatch | OnlyChangesToFiles
               | LeaveTestDir | NoLeaveTestDir
               | Timings | Debug | DebugVerbose | DebugHTTP
               | Verbose | NormalVerbosity | Quiet
               | Target String | Cc String
               | Output AbsolutePathOrStd | OutputAutoName AbsolutePath
               | Subject String | InReplyTo String
               | SendmailCmd String | Author String | PatchName String
               | OnePatch String | SeveralPatch String
               | AfterPatch String | UpToPatch String
               | TagName String | LastN Int | PatchIndexRange Int Int
               | NumberPatches
               | OneTag String | AfterTag String | UpToTag String
               | Context AbsolutePath | Count
               | LogFile AbsolutePath | RmLogFile
               | DistName String | All
               | Recursive | NoRecursive | Reorder
               | RestrictPaths | DontRestrictPaths
               | AskDeps | NoAskDeps | IgnoreTimes | LookForAdds | NoLookForAdds
               | AnyOrder | CreatorHash String
               | Intersection | Union | Complement
               | Sign | SignAs String | NoSign | SignSSL String
               | HappyForwarding
               | Verify AbsolutePath | VerifySSL AbsolutePath
               | EditDescription | NoEditDescription
               | Toks String
               | EditLongComment | NoEditLongComment | PromptLongComment
               | AllowConflicts | MarkConflicts | NoAllowConflicts
               | Boring | AllowCaseOnly | AllowWindowsReserved
               | ShowAutogenerated | HideAutogenerated
               | DontGrabDeps | DontPromptForDependencies | PromptForDependencies
               | Compress | NoCompress | UnCompress
               | WorkDir String | RepoDir String | RemoteRepo String
               | Reply String | ApplyAs String
               | MachineReadable | HumanReadable
               | Pipe | Interactive
               | DiffCmd String
               | ExternalMerge String | Summary | NoSummary
               | Unified | Reverse
               | Complete | Lazy | Ephemeral
               | FixFilePath AbsolutePath AbsolutePath | DiffFlags String
               | XMLOutput
               | ForceReplace
               | NonApply | NonVerify | NonForce
               | DryRun | SetDefault | NoSetDefault
               | FancyMoveAdd | NoFancyMoveAdd
               | Disable
               | UseHashedInventory | UseOldFashionedInventory
               | UseFormat2
               | PristinePlain | PristineNone | NoUpdateWorking
               | Sibling AbsolutePath | Relink | NoLinks
               | Files | NoFiles | Directories | NoDirectories
               | Pending | NoPending
               | PosthookCmd String | NoPosthook
               | PrehookCmd String  | NoPrehook
               | UMask String
               | StoreInMemory
               | HTTPPipelining | NoHTTPPipelining
               | NoCache
               | AllowUnrelatedRepos
               | NullFlag
                 deriving ( Eq, Show )

data Compression = NoCompression | GzipCompression
compression :: [ArcsFlag] -> Compression
compression f | NoCompress `elem` f = NoCompression
              | otherwise = GzipCompression

want_external_merge :: [ArcsFlag] -> Maybe String
want_external_merge [] = Nothing
want_external_merge (ExternalMerge c:_) = Just c
want_external_merge (_:fs) = want_external_merge fs

isInteractive :: [ArcsFlag] -> Bool
isInteractive = isInteractive_ True
    where
      isInteractive_ def [] = def
      isInteractive_ _ (Interactive:_) = True
      isInteractive_ _ (All:_) = False
      isInteractive_ _ (DryRun:fs) = isInteractive_ False fs
      isInteractive_ def (_:fs) = isInteractive_ def fs

