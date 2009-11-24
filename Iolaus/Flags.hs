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

module Iolaus.Flags ( Flag( .. ), isInteractive ) where
import Iolaus.RepoPath ( AbsolutePath, AbsolutePathOrStd )

-- | The 'Flag' type is a list of all flags that can ever be
-- passed to iolaus, or to one of its commands.
data Flag = Help | ListOptions | NoTest | Test | Build | TestParents
          | NoCauterizeAllHeads | CauterizeAllHeads | CommutePast Int
          | DeltaDebugWorkingSubset | RecordFor String
          | Nice | NotNice
          | HelpOnMatch | OnlyChangesToFiles
          | LeaveTestDir | NoLeaveTestDir
          | Timings | Debug | DebugVerbose
          | Verbose | NormalVerbosity | Quiet
          | Output AbsolutePathOrStd | OutputAutoName AbsolutePath
          | Author String | PatchName String
          | OnePatch String | SeveralPatch String
          | AfterPatch String | UpToPatch String
          | TagName String | LastN Int | PatchIndexRange Int Int
          | MaxC Int | Count
          | OneTag String | AfterTag String | UpToTag String
          | LogFile AbsolutePath | RmLogFile | All
          | Intersection | Union
          | Sign | SignAs String | NoSign
          | Verify AbsolutePath | VerifyAny | NonVerify
          | EditDescription | NoEditDescription
          | EditLongComment | NoEditLongComment | PromptLongComment
          | AllowConflicts | NoAllowConflicts
          | WorkDir String | RepoDir String | RemoteRepo String
          | Interactive
          | Summary | NoSummary
          | ShowMerges | HideMerges
          | ShowParents | ShowHash | NoShowHash | ShowTested | HideTested
          | Unified | Reverse | Graph
          | FixFilePath AbsolutePath AbsolutePath
          | DryRun | ConfigDefault | GlobalConfig | SystemConfig
          | Disable
          | Sibling AbsolutePath
          | Files | NoFiles | Directories | NoDirectories
          | UMask String
          | NullFlag
            deriving ( Eq, Show )

isInteractive :: [Flag] -> Bool
isInteractive = isInteractive_ True
    where
      isInteractive_ def [] = def
      isInteractive_ _ (Interactive:_) = True
      isInteractive_ _ (All:_) = False
      isInteractive_ _ (DryRun:fs) = isInteractive_ False fs
      isInteractive_ def (_:fs) = isInteractive_ def fs

