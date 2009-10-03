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

{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP #-}

-- | SlurpDirectory is intended to give a nice lazy way of traversing directory
-- trees.
module Iolaus.SlurpDirectoryInternal
                      ( Slurpy(..), SlurpyContents(..),
                        get_filehash, get_dirhash, get_fileEbit,
                        slurpies_to_map, map_to_slurpies,
                        empty_slurpy, filterSlurpyPaths,
                        slurp, slurp_unboring, co_slurp,
                        slurp_name, is_file, is_dir,
                        get_filecontents, get_dircontents, get_slurp,
                        slurp_removefile, slurp_removedir,
                        slurp_remove,
                        slurp_modfile, slurp_hasfile, slurp_hasdir,
                        slurp_has_anycase,
                        slurp_has, list_slurpy, list_slurpy_files,
                        get_path_list,
                        list_slurpy_dirs,
                        isFileReallySymlink,
                        doesFileReallyExist, doesDirectoryReallyExist,
                        SlurpMonad, withSlurpy, write_files,
                        writeSlurpy
                      ) where

import System.IO
import System.Directory hiding ( getCurrentDirectory, renameFile )
import Iolaus.Workaround ( getCurrentDirectory )
import Iolaus.Utils ( withCurrentDirectory, formatPath )
import Iolaus.RepoPath ( FilePathLike, toFilePath )
import System.IO.Unsafe ( unsafeInterleaveIO )
import Data.List ( isPrefixOf )
import Data.Bits ( (.&.) )
import Control.Monad ( when, guard )
import Data.Char ( toLower )
import System.Posix.Files ( getSymbolicLinkStatus, fileMode, ownerExecuteMode,
                            isRegularFile, isDirectory, isSymbolicLink )
import Data.Maybe ( catMaybes, isJust, maybeToList )
import Data.Map (Map)
import qualified Data.Map as Map

import Iolaus.SignalHandler ( tryNonSignal )
import Iolaus.IO ( ReadableDirectory(..), WriteableDirectory(..),
                   MonadCatchy(..), ExecutableBit(..) )

import Iolaus.ByteStringUtils
import qualified Data.ByteString as B

import Git.Plumbing ( Hash, Tree, Blob )

import Iolaus.FileName ( FileName, fn2fp, fp2fn, norm_path, break_on_dir,
                              own_name, super_name )

#include "impossible.h"

data Slurpy = Slurpy !FileName !SlurpyContents

slurpy_to_pair :: Slurpy -> (FileName, SlurpyContents)
slurpy_to_pair (Slurpy fn sc) = (fn, sc)

pair_to_slurpy :: (FileName, SlurpyContents) -> Slurpy
pair_to_slurpy = uncurry Slurpy

slurpies_to_map :: [Slurpy] -> Map FileName SlurpyContents
slurpies_to_map = Map.fromList . map slurpy_to_pair

map_to_slurpies :: Map FileName SlurpyContents -> [Slurpy]
map_to_slurpies = map pair_to_slurpy . Map.toList

data SlurpyContents = SlurpDir (Maybe (Hash Tree)) (Map FileName SlurpyContents)
                    | SlurpFile !ExecutableBit (Maybe (Hash Blob)) B.ByteString
                    | SlurpSymlink B.ByteString

get_filehash :: Slurpy -> Maybe (Hash Blob)
get_filehash (Slurpy _ (SlurpFile _ x _)) = x
get_filehash _ = Nothing

get_dirhash :: Slurpy -> Maybe (Hash Tree)
get_dirhash (Slurpy _ (SlurpDir x _)) = x
get_dirhash _ = Nothing

get_fileEbit :: Slurpy -> Maybe ExecutableBit
get_fileEbit (Slurpy _ (SlurpFile e _ _)) = Just e
get_fileEbit _ = Nothing

instance Show Slurpy where
    show (Slurpy fn (SlurpDir _ l)) =
        "Dir " ++ (fn2fp fn) ++ "\n" ++
              concat (map show $ map_to_slurpies l) ++ "End Dir " ++ (fn2fp fn) ++ "\n"
    show (Slurpy fn (SlurpFile _ _ _)) = "File " ++ (fn2fp fn) ++ "\n"
    show (Slurpy fn (SlurpSymlink _)) = "Symlink " ++ (fn2fp fn) ++ "\n"

mapSlurpyNames :: (FileName -> FileName) -> Slurpy -> Slurpy
mapSlurpyNames f = onSlurpy
  where onSlurpy (Slurpy fn sc) = Slurpy (f fn) (onSlurpyContents sc)
        onSlurpyContents (SlurpDir x sm) =
            SlurpDir x . slurpies_to_map . map onSlurpy . map_to_slurpies $ sm
        onSlurpyContents sf = sf

slurp :: FilePathLike p => p -> IO Slurpy
slurp_unboring :: (FilePath->Bool) -> FilePath -> IO Slurpy
empty_slurpy :: Slurpy
empty_slurpy = Slurpy (fp2fn ".") (SlurpDir Nothing Map.empty)
slurp_name :: Slurpy -> FilePath
is_file :: Slurpy -> Bool
is_dir :: Slurpy -> Bool

get_filecontents :: Slurpy -> B.ByteString
get_dircontents :: Slurpy -> [Slurpy]

instance Eq Slurpy where
    s1 == s2 = (slurp_name s1) == (slurp_name s2)
instance Ord Slurpy where
    s1 <= s2 = (slurp_name s1) <= (slurp_name s2)

data SlurpMonad a = SM ((Either String Slurpy)
                        -> Either String (Slurpy, a))
mksm :: (Slurpy -> Either String (Slurpy, a)) -> SlurpMonad a
mksm x = SM sm where sm (Left e) = Left e
                     sm (Right s) = x s

instance Functor SlurpMonad where
    fmap f m = m >>= return . f

instance Monad SlurpMonad where
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

instance MonadCatchy SlurpMonad where
    SM p `catchMe` SM q = SM sm
        where sm e = case p e of
                     Left _ -> q e
                     okay -> okay

instance ReadableDirectory SlurpMonad where
    mDoesDirectoryExist d = smDoesDirectoryExist d
    mDoesFileExist f = smDoesFileExist f
    mInCurrentDirectory = smInSlurpy
    mGetDirectoryContents = smGetDirContents
    mReadFilePS = smReadFilePS
    mReadFilePSs = smReadFilePSs

instance WriteableDirectory SlurpMonad where
    mWithCurrentDirectory = modifySubSlurpy
    mSetFileExecutable = smSetFileExecutable
    mWriteFilePS = smWriteFilePS
    mCreateDirectory = smCreateDirectory
    mRename = smRename
    mRemoveDirectory = smRemoveDirectory
    mRemoveFile = smRemoveFile

write_file :: Slurpy -> FileName -> IO ()
write_file s fn = case withSlurpy s $ smReadFilePS fn of
                     Left err -> fail err
                     Right (_, c) -> do
                       ensureDirectories (super_name fn)
                       mWriteFilePS fn c
                       
try_write_file :: Slurpy -> FilePath -> IO ()
try_write_file s fp = let fn = fp2fn fp in
  if slurp_hasfile fn s
      then write_file s fn
      else if slurp_hasdir fn s
               then ensureDirectories fn
               else return ()

ensureDirectories :: WriteableDirectory m => FileName -> m ()
ensureDirectories d = do
          isPar <- mDoesDirectoryExist d
          if isPar 
            then return ()
            else ensureDirectories (super_name d) >> (mCreateDirectory d)

write_files ::  Slurpy -> [FilePath] -> IO ()
write_files s fps = mapM_ (try_write_file s) fps

-- don't overwrite non-empty directories unless explicitly asked by
-- being passed "." (which always exists)
writeSlurpy :: Slurpy -> FilePath -> IO ()
writeSlurpy s d = do
  when (d /= ".") $ createDirectory d
  withCurrentDirectory d $ write_files s (list_slurpy s)

withSlurpy :: Slurpy -> SlurpMonad a -> Either String (Slurpy, a)
withSlurpy s (SM f) = f (Right s)

smDoesDirectoryExist :: FileName -> SlurpMonad Bool
smDoesDirectoryExist d = mksm $ \s -> (Right (s, slurp_hasdir d s))

smDoesFileExist :: FileName -> SlurpMonad Bool
smDoesFileExist f = mksm $ \s -> (Right (s, slurp_hasfile f s))

-- smInSlurpy doesn't make any changes to the subdirectory.
smInSlurpy :: FileName -> SlurpMonad a -> SlurpMonad a
smInSlurpy d job = mksm sm
    where sm s = case get_slurp d s of
                 Just s' | is_dir s' -> case withSlurpy s' job of
                                        Left e -> Left e
                                        Right (_,a) -> Right (s, a)
                 _ -> Left $ "smInSlurpy:  Couldn't find directory " ++
                             formatPath (fn2fp d)

fromSlurpFile :: FileName -> (Slurpy -> a) -> SlurpMonad a
fromSlurpFile f job = mksm sm
    where sm s = case get_slurp f s of
                 Just s' | is_file s' -> Right (s, job s')
                 _ -> Left $ "fromSlurpFile:  Couldn't find file " ++
                             formatPath (fn2fp f)

modifySubSlurpy :: FileName -> SlurpMonad a -> SlurpMonad a
modifySubSlurpy d job = mksm sm
    where sm s = case get_slurp_context d s of
                 Just (ctx, sub@(Slurpy _ (SlurpDir _ _))) ->
                     case withSlurpy sub job of
                     Left e -> Left e
                     Right (sub',a) -> Right (ctx sub', a)
                 _ -> Left $ "modifySubSlurpy:  Couldn't find directory " ++
                             formatPath (fn2fp d)

modifyFileSlurpy :: FileName -> (Slurpy -> Slurpy) -> SlurpMonad ()
modifyFileSlurpy f job = mksm sm
    where sm s = case get_slurp_context f s of
                 Just (ctx, sf@(Slurpy _ (SlurpFile _ _ _))) -> Right (ctx $ job sf, ())
                 _ -> Left $ "modifyFileSlurpy:  Couldn't find file " ++
                             formatPath (fn2fp f)

insertSlurpy :: FileName -> Slurpy -> SlurpMonad ()
insertSlurpy f news = mksm $ \s ->
                      if slurp_hasfile f s || slurp_hasdir f s || not (slurp_hasdir (super_name f) s)
                      then Left $ "Error creating file "++fn2fp f
                      else Right (addslurp f news s, ())

smReadFilePS :: FileName -> SlurpMonad B.ByteString
smReadFilePS f = fromSlurpFile f get_filecontents

smReadFilePSs :: FileName -> SlurpMonad [B.ByteString]
smReadFilePSs f = fromSlurpFile f (linesPS . get_filecontents)

smGetDirContents :: SlurpMonad [FileName]
smGetDirContents = mksm $ \s -> Right (s, map slurp_fn $ get_dircontents s)

smWriteFilePS :: FileName -> B.ByteString -> SlurpMonad ()
smWriteFilePS f ps = -- this implementation could be made rather more direct
                     -- and limited to a single pass down the Slurpy
                     modifyFileSlurpy f modf
                     `catchMe` insertSlurpy f sl
    where sl = Slurpy (own_name f) (SlurpFile NotExecutable Nothing ps)
          modf (Slurpy _ (SlurpFile e _ _)) =
              Slurpy (own_name f) (SlurpFile e Nothing ps)
          modf _ = impossible

smSetFileExecutable :: FileName -> ExecutableBit -> SlurpMonad ()
smSetFileExecutable f e = modifyFileSlurpy f modf
    where modf (Slurpy ff (SlurpFile _ x y)) = Slurpy ff (SlurpFile e x y)
          modf _ = impossible

smCreateDirectory :: FileName -> SlurpMonad ()
smCreateDirectory a = mksm sm
    where sm s = case slurp_adddir a s of
                 Just s' -> Right (s', ())
                 Nothing -> Left $ "Error creating directory "++fn2fp a

smRename :: FileName -> FileName -> SlurpMonad ()
smRename a b = mksm sm
    where sm s = case slurp_move a b s of
                 Just s' -> Right (s', ())
                 Nothing -> 
                     -- Workaround for some old patches having moves when the source file doesn't exist.
                     if (slurp_has a s)
                         then Left $ "Error moving "++fn2fp a++" to "++fn2fp b
                         else Right (s, ())

smRemove :: FileName -> SlurpMonad ()
smRemove f = mksm sm
    where sm s = case slurp_remove f s of
                 Nothing -> Left $ fn2fp f++" does not exist."
                 Just s' -> Right (s', ())

smRemoveFile :: FileName -> SlurpMonad ()
smRemoveFile f =
    do exists <- mDoesFileExist f
       if exists then smRemove f
                 else fail $ "File "++fn2fp f++" does not exist."

smRemoveDirectory :: FileName -> SlurpMonad ()
smRemoveDirectory f =
    do exists <- mDoesDirectoryExist f
       if exists then smRemove f
                 else fail $ "Directory "++fn2fp f++" does not exist."

-- | Here are a few access functions.
slurp_name (Slurpy n _) = fn2fp n
slurp_fn :: Slurpy -> FileName
slurp_fn (Slurpy n _) = n
slurp_setname :: FileName -> Slurpy -> Slurpy
slurp_setname f (Slurpy _ s) = Slurpy f s

is_file (Slurpy _ (SlurpFile _ _ _)) = True
is_file (Slurpy _ _) = False

is_dir (Slurpy _ (SlurpDir _ _)) = True
is_dir (Slurpy _ _) = False

get_filecontents (Slurpy _ (SlurpFile _ _ c)) = c
get_filecontents _ = bug "Can't get_filecontents on SlurpDir."

get_dircontents (Slurpy _ (SlurpDir _ c)) = map_to_slurpies c
get_dircontents _ = bug "Can't get_dircontents on SlurpFile."

isFileReallySymlink :: FilePath -> IO Bool
isFileReallySymlink f = do fs <- getSymbolicLinkStatus f
                           return (isSymbolicLink fs)

doesFileReallyExist :: FilePath -> IO Bool
doesFileReallyExist f = do fs <- getSymbolicLinkStatus f
                           return (isRegularFile fs)

doesDirectoryReallyExist :: FilePath -> IO Bool
doesDirectoryReallyExist f = do fs <- getSymbolicLinkStatus f
                                return (isDirectory fs)

-- |slurp is how we get a slurpy in the first place\ldots
slurp = slurp_unboring (\_->True) . toFilePath
slurp_unboring = genslurp
genslurp :: (FilePath -> Bool)
         -> FilePath -> IO Slurpy
genslurp nb dirname = do
    isdir <- doesDirectoryExist dirname
    ms <- if isdir
          then withCurrentDirectory dirname $
               do actualname <- getCurrentDirectory
                  genslurp_helper nb (reverse actualname) "" "."
          else do former_dir <- getCurrentDirectory
                  genslurp_helper nb (reverse former_dir) "" dirname
    case ms of
      Just s -> return s
      Nothing -> fail $ "Unable to read directory " ++ dirname ++
                 " (it appears to be neither file nor directory)"

unsafeInterleaveMapIO :: (a -> IO b) -> [a] -> IO [b]
unsafeInterleaveMapIO _ [] = return []
unsafeInterleaveMapIO f (x:xs)
 = do x' <- f x
      xs' <- unsafeInterleaveIO $ unsafeInterleaveMapIO f xs
      return (x':xs')

genslurp_helper :: (FilePath -> Bool)
                -> FilePath -> String -> String -> IO (Maybe Slurpy)
genslurp_helper nb formerdir fullpath dirname = do
    fs <- getSymbolicLinkStatus fulldirname
    if isRegularFile fs
     then do ls <- unsafeInterleaveIO $ B.readFile fulldirname
             let ex = if fileMode fs .&. ownerExecuteMode == 0
                      then NotExecutable
                      else IsExecutable
             return $ Just $ Slurpy (fp2fn dirname) $ SlurpFile ex Nothing ls
     else if isDirectory fs || (isSymbolicLink fs && dirname == ".")
          then do sl <- unsafeInterleaveIO $
                        do fnames <- getDirectoryContents fulldirname
                           unsafeInterleaveMapIO
                                             (\f -> genslurp_helper nb fulldirname'
                                              (fullpath///f) f)
                                             $ filter (nb . (fullpath///)) $ filter not_hidden fnames
                  return $ Just $ Slurpy (fp2fn dirname) $ SlurpDir Nothing $ slurpies_to_map $ catMaybes sl
          else return Nothing
    where fulldirname' = formerdir\\\dirname
          fulldirname = reverse fulldirname'

not_hidden :: FilePath -> Bool
not_hidden "." = False
not_hidden ".." = False
not_hidden _ = True

(\\\) :: FilePath -> FilePath -> FilePath
(\\\) "" d = d
(\\\) d "." = d
(\\\) d subdir = reverse subdir ++ "/" ++ d

(///) :: FilePath -> FilePath -> FilePath
"" /// d = d
"." /// d = d
d /// "." = d
d /// subdir = d ++ "/" ++ subdir

co_slurp :: Slurpy -> FilePath -> IO Slurpy
co_slurp guide dirname = do
    isdir <- doesDirectoryExist dirname
    if isdir
       then withCurrentDirectory dirname $ do
              actualname <- getCurrentDirectory
              Just slurpy <- co_slurp_helper (reverse actualname) guide
              return slurpy
       else error "Error coslurping!!! Please report this."

co_slurp_helper :: FilePath -> Slurpy -> IO (Maybe Slurpy)
co_slurp_helper former_dir (Slurpy d (SlurpDir _ c)) = unsafeInterleaveIO $ do
    let d' = fn2fp d
        fn' = former_dir\\\d'
        fn = reverse fn'
    efs <- tryNonSignal $ getSymbolicLinkStatus fn
    case efs of
        Right fs
         | isDirectory fs || (isSymbolicLink fs && d' == ".") ->
            do sl <- unsafeInterleaveIO
                   $ unsafeInterleaveMapIO (co_slurp_helper fn') (map_to_slurpies c)
               return $ Just $ Slurpy d $ SlurpDir Nothing $ slurpies_to_map $ catMaybes sl
        _ -> return Nothing
co_slurp_helper former_dir (Slurpy f _) = unsafeInterleaveIO $ do
   let fn' = former_dir\\\fn2fp f
       fn = reverse fn'
   efs <- tryNonSignal $ getSymbolicLinkStatus fn
   case efs of
       Right fs
        | isRegularFile fs ->
           do ls <- unsafeInterleaveIO $ B.readFile fn
              return $ Just $ Slurpy f $ SlurpFile NotExecutable Nothing ls
       _ -> return Nothing

get_slurp_context_generic :: (Slurpy -> a) -> (a -> [Slurpy]) -> FileName -> Slurpy -> Maybe (a -> a, Slurpy)
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
get_slurp_context :: FileName -> Slurpy -> Maybe (Slurpy -> Slurpy, Slurpy)
get_slurp_context = get_slurp_context_generic id return

-- |A variant of 'get_slurp_context' that allows for removing the child slurpy
-- altogether by passing in 'Nothing' to the update function.
-- If the child slurpy happened to be at the top level and 'Nothing' was passed in,
-- then the result of the update function will also be 'Nothing', otherwise it will always
-- be a 'Just' value.
get_slurp_context_maybe :: FileName -> Slurpy -> Maybe (Maybe Slurpy -> Maybe Slurpy, Slurpy)
get_slurp_context_maybe = get_slurp_context_generic Just maybeToList

-- |A variant of 'get_slurp_context' that allows for replacing the child slurpy by
-- a list of slurpies. The result of the update function will always be a singleton
-- list unless the child slurpy was at the top level.
-- Currently unused.
-- get_slurp_context_list :: FileName -> Slurpy -> Maybe ([Slurpy] -> [Slurpy], Slurpy)
-- get_slurp_context_list = get_slurp_context_generic return id

-- | It is important to be able to readily modify a slurpy.
slurp_remove :: FileName -> Slurpy -> Maybe Slurpy
slurp_remove fname s@(Slurpy _ (SlurpDir _ _)) =
    case get_slurp_context_maybe fname s of
        Just (ctx, _) -> ctx Nothing
        Nothing -> Nothing
slurp_remove _ _ = bug "slurp_remove only acts on SlurpDirs"

slurp_removefile :: FileName -> Slurpy -> Maybe Slurpy
slurp_removefile f s =
  if slurp_hasfile f s
  then case slurp_remove f s of
       s'@(Just (Slurpy _ (SlurpDir _ _))) -> s'
       _ -> impossible
  else Nothing

slurp_move :: FileName -> FileName -> Slurpy -> Maybe Slurpy
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

addslurp :: FileName -> Slurpy -> Slurpy -> Slurpy
addslurp fname s s' =
    case get_slurp_context (super_name fname) s' of
        Just (ctx, Slurpy d (SlurpDir _ c)) -> ctx (Slurpy d (SlurpDir Nothing (uncurry Map.insert (slurpy_to_pair s) c)))
        _ -> s'

get_slurp :: FileName -> Slurpy -> Maybe Slurpy
get_slurp f s = fmap snd (get_slurp_context f s)

slurp_removedir :: FileName -> Slurpy -> Maybe Slurpy
slurp_removedir f s =
    case get_slurp f s of
    Just (Slurpy _ (SlurpDir _ l)) | Map.null l ->
        case slurp_remove f s of
        s'@(Just (Slurpy _ (SlurpDir _ _))) -> s'
        _ -> impossible
    _ -> Nothing

slurp_adddir :: FileName -> Slurpy -> Maybe Slurpy
slurp_adddir f s =
  if slurp_hasfile f s || slurp_hasdir f s || not (slurp_hasdir (super_name f) s)
  then Nothing
  else Just $ addslurp f (Slurpy (own_name f) (SlurpDir Nothing Map.empty)) s

-- |Code to modify a given file in a slurpy.
slurp_modfile :: FileName -> (B.ByteString -> Maybe B.ByteString)
              -> Slurpy -> Maybe Slurpy
slurp_modfile fname modify sl =
    case get_slurp_context fname sl of
        Just (ctx, Slurpy ff (SlurpFile _ _ c)) ->
            case modify c of
                Nothing -> Nothing
                Just c' -> Just (ctx (Slurpy ff (SlurpFile NotExecutable Nothing c')))
        _ -> 
            Nothing

slurp_hasfile :: FileName -> Slurpy -> Bool
slurp_hasfile f s =
    case get_slurp f s of
        Just s' | is_file s' -> True
        _ -> False

slurp_has :: FileName -> Slurpy -> Bool
slurp_has f s = isJust (get_slurp f s)

slurp_has_anycase :: FilePath -> Slurpy -> Bool
slurp_has_anycase fname s =
  seq normed_name $ isJust $ get_slurp normed_name $ mapSlurpyNames tolower s
  where normed_name = norm_path $ fp2fn $ map toLower fname

tolower :: FileName -> FileName
tolower = fp2fn . (map toLower) . fn2fp

findSubSlurpy :: FileName -> Map FileName SlurpyContents -> Maybe Slurpy
findSubSlurpy fn sm =
  let topname = case break_on_dir fn of
                   Just (dn, _) -> dn
                   Nothing -> fn
  in fmap (Slurpy topname) (Map.lookup topname sm)

slurp_hasdir :: FileName -> Slurpy -> Bool
slurp_hasdir d _ | norm_path d == fp2fn "" = True
slurp_hasdir f (Slurpy _ (SlurpDir _ c)) =
    seq f $ let f' = norm_path f
            in case findSubSlurpy f' c of
                Just s -> slurp_hasdir_private f' s
                Nothing -> False
slurp_hasdir _ _ = False

slurp_hasdir_private :: FileName -> Slurpy -> Bool
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

get_path_list :: Slurpy -> FilePath -> [FilePath]
get_path_list s fp = get_path_list' s ("./" ++ fp)

get_path_list' :: Slurpy -> FilePath -> [FilePath]
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

list_slurpy :: Slurpy -> [FilePath]
list_slurpy (Slurpy dd (SlurpDir _ ss)) = d : map (d ///) (concatMap list_slurpy (map_to_slurpies ss))
    where d = fn2fp dd
list_slurpy (Slurpy f _) = [fn2fp f]

list_slurpy_files :: Slurpy -> [FilePath]
list_slurpy_files (Slurpy dd (SlurpDir _ ss)) =
    map ((fn2fp dd) ///) (concatMap list_slurpy_files (map_to_slurpies ss))
list_slurpy_files (Slurpy f _) = [fn2fp f]

list_slurpy_dirs :: Slurpy -> [FilePath]
list_slurpy_dirs (Slurpy dd (SlurpDir _ ss)) =
    d : map (d ///) (concatMap list_slurpy_dirs (map_to_slurpies ss))
    where d = fn2fp dd
list_slurpy_dirs _ = []

filterSlurpyPaths :: [FileName] -> Slurpy -> Slurpy
filterSlurpyPaths [] s = s
filterSlurpyPaths _ s@(Slurpy _ (SlurpFile _ _ _)) = s
filterSlurpyPaths _ s@(Slurpy _ (SlurpSymlink _)) = s
filterSlurpyPaths f (Slurpy d (SlurpDir _ c)) = Slurpy d (SlurpDir Nothing c')
    where c' = updateAll f c

filterSlurpyPaths' :: [FileName] -> Slurpy -> Maybe Slurpy
filterSlurpyPaths' [] _ = Nothing
filterSlurpyPaths' f s@(Slurpy fn _) | fn `elem` f = Just s
filterSlurpyPaths' f (Slurpy d (SlurpDir _ c)) = do guard $ not $ null f'
                                                    Just (Slurpy d (SlurpDir Nothing c'))
    where c' = updateAll f' c
          f' = map snd $ filter ((==d).fst) $ catMaybes (map break_on_dir f)
filterSlurpyPaths' _ _ = Nothing

updateAll :: [FileName] -> Map FileName SlurpyContents
          -> Map FileName SlurpyContents
updateAll f m = Map.map (\ (Just (Slurpy _ s)) -> s) $ Map.filter isJust $
                Map.mapWithKey (\k x -> filterSlurpyPaths' f $ Slurpy k x) m
