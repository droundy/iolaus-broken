-- Copyright (C) 2003 David Roundy
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

module Iolaus.Commands ( command_control_list ) where

import Iolaus.Commands.Record ( record )
import Iolaus.Commands.Amend ( amend_record )
import Iolaus.Commands.Unrecord ( unrecord )
import Iolaus.Commands.Whatsnew ( whatsnew )
import Iolaus.Commands.Changes ( changes )
import Iolaus.Commands.Get ( get )
import Iolaus.Commands.Pull ( pull )
import Iolaus.Commands.Push ( push )
import Iolaus.Commands.Initialize ( initialize )
import Iolaus.Commands.Show ( show_command )
import Iolaus.Commands.Revert ( revert )
import Iolaus.Commands.Unrevert ( unrevert )
import Iolaus.Command ( CommandControl( Command_data, -- Hidden_command,
                                        Group_name) )

-- | The commands that arcs knows about (e.g. whatsnew, record),
--   organized into thematic groups.  Note that hidden commands
--   are also listed here.
command_control_list :: [CommandControl]
command_control_list =
    [Group_name "Changing and querying the working copy:",
     Command_data whatsnew,
     Command_data changes,
     Command_data unrecord,
     Command_data record,
     Command_data amend_record,
     Command_data revert,
     Command_data unrevert,
     Command_data show_command,
     Command_data initialize,

     Group_name "Interacting with other repositories:",
     Command_data get,
     Command_data push,
     Command_data pull
    ]
