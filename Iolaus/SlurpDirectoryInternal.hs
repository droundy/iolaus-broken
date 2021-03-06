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
{-# LANGUAGE CPP #-}
#include "gadts.h"

-- | SlurpDirectory is intended to give a nice lazy way of traversing directory
-- trees.
module Iolaus.SlurpDirectoryInternal
                      ( Slurpy(..), SlurpyContents(..), subslurpies,
                        get_filehash, get_dirhash, get_fileEbit,
                        slurpies_to_map, map_to_slurpies,
                        empty_slurpy, filterSlurpyPaths,
                        slurp_name, is_file, is_dir,
                        get_filecontents, get_dircontents, get_slurp,
                        list_slurpy, list_slurpy_files, get_path_list,
                        list_slurpy_dirs,
                        doesFileReallyExist, doesDirectoryReallyExist,
                        SlurpMonad, withSlurpy
                      ) where

import System.IO
import Data.List ( isPrefixOf )
import Control.Monad ( guard )
import System.Posix.Files ( getSymbolicLinkStatus, isRegularFile, isDirectory )
import Data.Maybe ( catMaybes, isJust, maybeToList )
import Data.Map (Map)
import qualified Data.Map as Map

import Iolaus.Utils ( formatPath )
import Iolaus.IO ( WriteableDirectory(..), ExecutableBit(..) )
import Iolaus.Show ( Ord1(..), Eq1(..), Show1(..) )
import qualified Data.ByteString as B

import Git.Plumbing ( Hash, Tree, Blob )

import Iolaus.FileName ( FileName, fn2fp, fp2fn, norm_path, break_on_dir,
                              own_name, super_name )

#include "impossible.h"

data Slurpy C(x) = Slurpy !FileName !(SlurpyContents C(x))

slurpy_to_pair :: Slurpy C(x) -> (FileName, SlurpyContents C(x))
slurpy_to_pair (Slurpy fn sc) = (fn, sc)

pair_to_slurpy :: (FileName, SlurpyContents C(x)) -> Slurpy C(x)
pair_to_slurpy = uncurry Slurpy

slurpies_to_map :: [Slurpy C(x)] -> Map FileName (SlurpyContents C(x)) 
slurpies_to_map = Map.fromList . map slurpy_to_pair

map_to_slurpies :: Map FileName (SlurpyContents C(x)) -> [Slurpy C(x)]
map_to_slurpies = map pair_to_slurpy . Map.toList

data SlurpyContents C(x) = SlurpDir (Maybe (Hash Tree C(x)))
                                    (Map FileName (SlurpyContents C(x)))
                         | SlurpFile !ExecutableBit
                           (Maybe (Hash Blob C(x))) B.ByteString
                         | SlurpSymlink B.ByteString

get_filehash :: Slurpy C(x) -> Maybe (Hash Blob C(x))
get_filehash (Slurpy _ (SlurpFile _ x _)) = x
get_filehash _ = Nothing

get_dirhash :: Slurpy C(x) -> Maybe (Hash Tree C(x))
get_dirhash (Slurpy _ (SlurpDir x _)) = x
get_dirhash _ = Nothing

get_fileEbit :: Slurpy C(x) -> Maybe ExecutableBit
get_fileEbit (Slurpy _ (SlurpFile e _ _)) = Just e
get_fileEbit _ = Nothing

instance Show1 Slurpy where
    show1 (Slurpy fn (SlurpDir _ l)) =
        "Dir " ++ (fn2fp fn) ++ "\n" ++
              concat (map show1 $ map_to_slurpies l) ++ "End Dir " ++ (fn2fp fn) ++ "\n"
    show1 (Slurpy fn (SlurpFile _ _ _)) = "File " ++ (fn2fp fn) ++ "\n"
    show1 (Slurpy fn (SlurpSymlink _)) = "Symlink " ++ (fn2fp fn) ++ "\n"
instance Show (Slurpy C(x)) where show = show1

empty_slurpy :: Slurpy C(x)
empty_slurpy = Slurpy (fp2fn ".") (SlurpDir Nothing Map.empty)

slurp_name :: Slurpy C(x) -> FilePath
is_file :: Slurpy C(x) -> Bool
is_dir :: Slurpy C(x) -> Bool

get_filecontents :: Slurpy C(x) -> B.ByteString
get_dircontents :: Slurpy C(x) -> [Slurpy C(x)]

instance Eq1 Slurpy where
    eq1 s1 s2 = (slurp_name s1) == (slurp_name s2)
instance Eq (Slurpy C(x)) where (==) = eq1
instance Ord1 Slurpy where
    compare1 s1 s2 = compare (slurp_name s1) (slurp_name s2)
instance Ord (Slurpy C(x)) where compare = compare1

data SlurpMonad C(x y) a = SM (Either String (Slurpy C(x))
                               -> Either String (Slurpy C(y), a))
mksm :: (Slurpy C(x) -> Either String (Slurpy C(y), a))
     -> SlurpMonad C(x y) a
mksm x = SM sm where sm (Left e) = Left e
                     sm (Right s) = x s

instance Functor (SlurpMonad C(x x)) where
    fmap f m = m >>= return . f

instance Monad (SlurpMonad C(x x)) where
    (SM p) >>= k  =  SM sm
        where sm e = case p e of
                     Left er -> Left er
                     Right (s, a) -> case k a of
                                     (SM q) -> q (Right s)
    return a = SM ( \s -> case s of
                          Left e -> Left e
                          Right x -> Right (x, a) )
    fail e = SM ( \s -> case s of
                        Left x -> Left x
                        _ -> Left e )

instance WriteableDirectory (SlurpMonad C(x x)) where
    mDoesDirectoryExist d = smDoesDirectoryExist d
    mDoesFileExist f = smDoesFileExist f
    mReadFilePS = smReadFilePS
    mSetFileExecutable = smSetFileExecutable
    mWriteFilePS = smWriteFilePS
    mCreateDirectory = smCreateDirectory
    mRename = smRename
    mRemoveDirectory = smRemoveDirectory
    mRemoveFile = smRemoveFile

withSlurpy :: Slurpy C(x) -> SlurpMonad C(x x) a
           -> Either String (Slurpy C(x), a)
withSlurpy s (SM f) = f (Right s)

smDoesDirectoryExist :: FileName -> SlurpMonad C(x x) Bool
smDoesDirectoryExist d = mksm $ \s -> (Right (s, slurp_hasdir d s))

smDoesFileExist :: FileName -> SlurpMonad C(x x) Bool
smDoesFileExist f = mksm $ \s -> (Right (s, slurp_hasfile f s))

fromSlurpFile :: FileName -> (Slurpy C(x) -> a) -> SlurpMonad C(x x) a
fromSlurpFile f job = mksm sm
    where sm s = case get_slurp f s of
                 Just s' | is_file s' -> Right (s, job s')
                 _ -> Left $ "fromSlurpFile:  Couldn't find file " ++
                             formatPath (fn2fp f)

modifyFileSlurpy :: FileName -> (Slurpy C(x) -> Slurpy C(x))
                 -> SlurpMonad C(x x) ()
modifyFileSlurpy f job = mksm sm
    where sm s = case get_slurp_context f s of
                 Just (ctx, sf@(Slurpy _ (SlurpFile _ _ _))) -> Right (ctx $ job sf, ())
                 _ -> Left $ "modifyFileSlurpy:  Couldn't find file " ++
                             formatPath (fn2fp f)

insertSlurpy :: FileName -> Slurpy C(x) -> SlurpMonad C(x x) ()
insertSlurpy f news = mksm $ \s ->
                      if slurp_hasfile f s || slurp_hasdir f s || not (slurp_hasdir (super_name f) s)
                      then Left $ "Error creating file "++fn2fp f
                      else Right (addslurp f news s, ())

smReadFilePS :: FileName -> SlurpMonad C(x x) B.ByteString
smReadFilePS f = fromSlurpFile f get_filecontents

smWriteFilePS :: FileName -> B.ByteString -> SlurpMonad C(x x) ()
smWriteFilePS f ps = -- this implementation could be made rather more direct
                     -- and limited to a single pass down the Slurpy
                     modifyFileSlurpy f modf
                     `catchMe` insertSlurpy f sl
    where sl = Slurpy (own_name f) (SlurpFile NotExecutable Nothing ps)
          modf (Slurpy _ (SlurpFile e _ _)) =
              Slurpy (own_name f) (SlurpFile e Nothing ps)
          modf _ = impossible
          SM p `catchMe` SM q = SM sm
              where sm e = case p e of Left _ -> q e
                                       okay -> okay



smSetFileExecutable :: FileName -> ExecutableBit -> SlurpMonad C(x x) ()
smSetFileExecutable f e = modifyFileSlurpy f modf
    where modf (Slurpy ff (SlurpFile _ x y)) = Slurpy ff (SlurpFile e x y)
          modf _ = impossible

smCreateDirectory :: FileName -> SlurpMonad C(x x) ()
smCreateDirectory a = mksm sm
    where sm s = case slurp_adddir a s of
                 Just s' -> Right (s', ())
                 Nothing -> Left $ "Error creating directory "++fn2fp a

smRename :: FileName -> FileName -> SlurpMonad C(x x) ()
smRename a b = mksm sm
    where sm s = case slurp_move a b s of
                 Just s' -> Right (s', ())
                 Nothing -> 
                     -- Workaround for some old patches having moves when the source file doesn't exist.
                     if (slurp_has a s)
                         then Left $ "Error moving "++fn2fp a++" to "++fn2fp b
                         else Right (s, ())

smRemove :: FileName -> SlurpMonad C(x x) ()
smRemove f = mksm sm
    where sm s = case slurp_remove f s of
                 Nothing -> Left $ fn2fp f++" does not exist."
                 Just s' -> Right (s', ())

smRemoveFile :: FileName -> SlurpMonad C(x x) ()
smRemoveFile f =
    do exists <- mDoesFileExist f
       if exists then smRemove f
                 else fail $ "File "++fn2fp f++" does not exist."

smRemoveDirectory :: FileName -> SlurpMonad C(x x) ()
smRemoveDirectory f =
    do exists <- mDoesDirectoryExist f
       if exists then smRemove f
                 else fail $ "Directory "++fn2fp f++" does not exist."

-- | Here are a few access functions.
slurp_name (Slurpy n _) = fn2fp n
slurp_fn :: Slurpy C(x) -> FileName
slurp_fn (Slurpy n _) = n
slurp_setname :: FileName -> Slurpy C(x) -> Slurpy C(x)
slurp_setname f (Slurpy _ s) = Slurpy f s

is_file (Slurpy _ (SlurpFile _ _ _)) = True
is_file (Slurpy _ _) = False

is_dir (Slurpy _ (SlurpDir _ _)) = True
is_dir (Slurpy _ _) = False

get_filecontents (Slurpy _ (SlurpFile _ _ c)) = c
get_filecontents _ = bug "Can't get_filecontents on SlurpDir."

get_dircontents (Slurpy _ (SlurpDir _ c)) = map_to_slurpies c
get_dircontents x = bug ("Can't get_dircontents on SlurpFile. "++show x)

doesFileReallyExist :: FilePath -> IO Bool
doesFileReallyExist f = do fs <- getSymbolicLinkStatus f
                           return (isRegularFile fs)

doesDirectoryReallyExist :: FilePath -> IO Bool
doesDirectoryReallyExist f = do fs <- getSymbolicLinkStatus f
                                return (isDirectory fs)

(///) :: FilePath -> FilePath -> FilePath
"" /// d = d
"." /// d = d
d /// "." = d
d /// subdir = d ++ "/" ++ subdir

get_slurp_context_generic :: (Slurpy C(x) -> a)
                          -> (a -> [Slurpy C(x)])
                          -> FileName -> Slurpy C(x)
                          -> Maybe (a -> a, Slurpy C(x))
get_slurp_context_generic h1 h2 fn0 s0 =
    let norm_fn0 = norm_path fn0 in
    if norm_fn0 == empty
        then Just (id, s0)
        else slurp_context_private norm_fn0 id s0
  where
    slurp_context_private f ctx s@(Slurpy d (SlurpDir _ c))
      | f == d = Just (ctx, s)
      | d == dot =
            case break_on_dir f of
                Just (dn,fn) | dn == dot ->
                    descend fn
                _ ->
                    descend f
      | otherwise =
            case break_on_dir f of
                Just (dn,fn) ->
                    if dn == d
                        then descend fn
                        else Nothing
                _ -> Nothing
      where
        descend fname =
            case findSubSlurpy fname c of
                  Nothing -> Nothing
                  Just this -> slurp_context_private
                                   fname
                                   (ctx . h1 . Slurpy d . SlurpDir Nothing . foldr (uncurry Map.insert) (Map.delete (slurp_fn this) c) . map slurpy_to_pair . h2)
                                   this

    slurp_context_private f ctx s@(Slurpy f' _) = if f == f' then Just (ctx, s)
                                                             else Nothing
    dot = fp2fn "."
    empty = fp2fn ""

-- |get_slurp_context navigates to a specified filename in the given slurpy,
-- and returns the child slurpy at that point together with a update function that can be used
-- to reconstruct the original slurpy from a replacement value for the child slurpy.
get_slurp_context :: FileName -> Slurpy C(x)
                  -> Maybe (Slurpy C(x) -> Slurpy C(x), Slurpy C(x))
get_slurp_context = get_slurp_context_generic id return

-- |A variant of 'get_slurp_context' that allows for removing the child slurpy
-- altogether by passing in 'Nothing' to the update function.
-- If the child slurpy happened to be at the top level and 'Nothing' was passed in,
-- then the result of the update function will also be 'Nothing', otherwise it will always
-- be a 'Just' value.
get_slurp_context_maybe :: FileName -> Slurpy C(x)
                        -> Maybe (Maybe (Slurpy C(x))
                                      -> Maybe (Slurpy C(x)), Slurpy C(x))
get_slurp_context_maybe = get_slurp_context_generic Just maybeToList

-- |A variant of 'get_slurp_context' that allows for replacing the child slurpy by
-- a list of slurpies. The result of the update function will always be a singleton
-- list unless the child slurpy was at the top level.
-- Currently unused.
-- get_slurp_context_list :: FileName -> Slurpy C(x) -> Maybe ([Slurpy] -> [Slurpy], Slurpy)
-- get_slurp_context_list = get_slurp_context_generic return id

-- | It is important to be able to readily modify a slurpy.
slurp_remove :: FileName -> Slurpy C(x) -> Maybe (Slurpy C(x))
slurp_remove fname s@(Slurpy _ (SlurpDir _ _)) =
    case get_slurp_context_maybe fname s of
        Just (ctx, _) -> ctx Nothing
        Nothing -> Nothing
slurp_remove _ _ = bug "slurp_remove only acts on SlurpDirs"

slurp_move :: FileName -> FileName -> Slurpy C(x) -> Maybe (Slurpy C(x))
slurp_move f f' s =
    if not (slurp_has f' s) && slurp_hasdir (super_name f') s
    then case get_slurp f s of
         Nothing -> Nothing
         Just sf ->
             case slurp_remove f s of
             Nothing -> Nothing
             Just (s'@(Slurpy _ (SlurpDir _ _))) ->
                 Just $ addslurp f' (slurp_setname (own_name f') sf) s'
             _ -> impossible
    else Nothing

addslurp :: FileName -> Slurpy C(x) -> Slurpy C(x) -> Slurpy C(x)
addslurp fname s s' =
    case get_slurp_context (super_name fname) s' of
        Just (ctx, Slurpy d (SlurpDir _ c)) ->
            ctx (Slurpy d (SlurpDir Nothing (uncurry Map.insert
                                                         (slurpy_to_pair s) c)))
        _ -> s'

get_slurp :: FileName -> Slurpy C(x) -> Maybe (Slurpy C(x))
get_slurp f s = fmap snd (get_slurp_context f s)

slurp_adddir :: FileName -> Slurpy C(x) -> Maybe (Slurpy C(x))
slurp_adddir f s =
  if slurp_hasfile f s || slurp_hasdir f s || not (slurp_hasdir (super_name f) s)
  then Nothing
  else Just $ addslurp f (Slurpy (own_name f) (SlurpDir Nothing Map.empty)) s

slurp_hasfile :: FileName -> Slurpy C(x) -> Bool
slurp_hasfile f s =
    case get_slurp f s of
        Just s' | is_file s' -> True
        _ -> False

slurp_has :: FileName -> Slurpy C(x) -> Bool
slurp_has f s = isJust (get_slurp f s)

findSubSlurpy :: FileName -> Map FileName (SlurpyContents C(x))
              -> Maybe (Slurpy C(x))
findSubSlurpy fn sm =
  let topname = case break_on_dir fn of
                   Just (dn, _) -> dn
                   Nothing -> fn
  in fmap (Slurpy topname) (Map.lookup topname sm)

slurp_hasdir :: FileName -> Slurpy C(x) -> Bool
slurp_hasdir d _ | norm_path d == fp2fn "" = True
slurp_hasdir f (Slurpy _ (SlurpDir _ c)) =
    seq f $ let f' = norm_path f
            in case findSubSlurpy f' c of
                Just s -> slurp_hasdir_private f' s
                Nothing -> False
slurp_hasdir _ _ = False

slurp_hasdir_private :: FileName -> Slurpy C(x) -> Bool
slurp_hasdir_private f (Slurpy d (SlurpDir _ c))
  | f == d = True
  | otherwise =
       case break_on_dir f of
       Just (dn,fn) ->
           if dn == d
           then case findSubSlurpy fn c of
                   Just s -> slurp_hasdir_private fn s
                   Nothing -> False
           else False
       _ -> False
slurp_hasdir_private _ (Slurpy _ _) = False

get_path_list :: Slurpy C(x) -> FilePath -> [FilePath]
get_path_list s fp = get_path_list' s ("./" ++ fp)

get_path_list'
    :: Slurpy C(x) -> FilePath -> [FilePath]
get_path_list' s "" = list_slurpy s
get_path_list' (Slurpy f (SlurpFile _ _ _)) fp
 | f' == fp = [f']
    where f' = fn2fp f
get_path_list' (Slurpy d (SlurpDir _ ss)) fp
 | (d' ++ "/") `isPrefixOf` (fp ++ "/")
    = let fp' = drop (length d' + 1) fp
      in map (d' ///) $ concatMap (\s -> get_path_list' s fp') $ map_to_slurpies ss
    where d' = fn2fp d
get_path_list' _ _ = []

subslurpies :: Slurpy C(x) -> [(FilePath, Slurpy C(x))]
subslurpies s@(Slurpy dd (SlurpDir _ ss)) =
    (d,s) : map (\ (d',s') -> (d ///d',s'))
            (concatMap subslurpies (map_to_slurpies ss))
    where d = fn2fp dd
subslurpies s@(Slurpy f _) = [(fn2fp f, s)]

list_slurpy :: Slurpy C(x) -> [FilePath]
list_slurpy (Slurpy dd (SlurpDir _ ss)) = d : map (d ///) (concatMap list_slurpy (map_to_slurpies ss))
    where d = fn2fp dd
list_slurpy (Slurpy f _) = [fn2fp f]

list_slurpy_files :: Slurpy C(x) -> [FilePath]
list_slurpy_files (Slurpy dd (SlurpDir _ ss)) =
    map ((fn2fp dd) ///) (concatMap list_slurpy_files (map_to_slurpies ss))
list_slurpy_files (Slurpy f _) = [fn2fp f]

list_slurpy_dirs :: Slurpy C(x) -> [FilePath]
list_slurpy_dirs (Slurpy dd (SlurpDir _ ss)) =
    d : map (d ///) (concatMap list_slurpy_dirs (map_to_slurpies ss))
    where d = fn2fp dd
list_slurpy_dirs _ = []

filterSlurpyPaths :: [FileName] -> Slurpy C(x) -> Slurpy C(x)
filterSlurpyPaths [] s = s
filterSlurpyPaths _ s@(Slurpy _ (SlurpFile _ _ _)) = s
filterSlurpyPaths _ s@(Slurpy _ (SlurpSymlink _)) = s
filterSlurpyPaths f (Slurpy d (SlurpDir _ c)) = Slurpy d (SlurpDir Nothing c')
    where c' = updateAll f c

filterSlurpyPaths'
    :: [FileName] -> Slurpy C(x) -> Maybe (Slurpy C(x))
filterSlurpyPaths' [] _ = Nothing
filterSlurpyPaths' f s@(Slurpy fn _) | fn `elem` f = Just s
filterSlurpyPaths' f (Slurpy d (SlurpDir _ c)) = do guard $ not $ null f'
                                                    Just (Slurpy d (SlurpDir Nothing c'))
    where c' = updateAll f' c
          f' = map snd $ filter ((==d).fst) $ catMaybes (map break_on_dir f)
filterSlurpyPaths' _ _ = Nothing

updateAll :: [FileName] -> Map FileName (SlurpyContents C(x))
          -> Map FileName (SlurpyContents C(x))
updateAll f m = Map.map (\ (Just (Slurpy _ s)) -> s) $ Map.filter isJust $
                Map.mapWithKey (\k x -> filterSlurpyPaths' f $ Slurpy k x) m
