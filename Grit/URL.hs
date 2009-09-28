{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP #-}

{-
Copyright (C) 2004 David Roundy

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.
-}

{-|

  Path resolving:

    * A URL contains the sequence @\":\/\/\"@.

    * A local filepath does not contain colons, except
      as second character (windows drives).

    * A path that is neither a URL nor a local file
      is an ssh-path.

  Examples:

  > /usr/repo/foo                 -- local file
  > c:/src/darcs                  -- local file
  > http://darcs.net/             -- URL
  > peter@host:/path              -- ssh
  > droundy@host:                 -- ssh
  > host:/path                    -- ssh

  This means that single-letter hosts in ssh-paths do not work,
  unless a username is provided.

  Perhaps ssh-paths should use @\"ssh:\/\/user\@host\/path\"@-syntax instead?
-}

module Grit.URL (
    is_file, is_url, is_ssh, is_relative, is_absolute,
    is_ssh_nopath
  ) where

is_relative :: String -> Bool
is_relative (_:':':_) = False
is_relative f@(c:_) = is_file f && c /= '/' && c /= '~'
is_relative "" = False

is_absolute :: String -> Bool
is_absolute f = is_file f && (not $ is_relative f)

is_file :: String -> Bool
is_file (_:_:fou) = ':' `notElem` fou
is_file _ = True

is_url :: String -> Bool
is_url (':':'/':'/':_:_) = True
is_url (_:x) = is_url x
is_url "" = False

is_ssh :: String -> Bool
is_ssh s = not (is_file s || is_url s)

is_ssh_nopath :: String -> Bool
is_ssh_nopath s = case reverse s of
                  ':':x@(_:_:_) -> ':' `notElem` x
                  _ -> False
