%  Copyright (C) 2002-2005 David Roundy
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
{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

#include "gadts.h"

module Iolaus.Patch.Apply ( apply_to_slurpy, applyFL ) where

import Prelude hiding ( catch, pi )

import qualified Data.ByteString.Char8 as BC ( singleton )
import qualified Data.ByteString as B ( ByteString, null, empty, concat )

import Iolaus.ByteStringUtils ( unlinesPS,
                              break_after_nth_newline,
                              break_before_nth_newline )
import Iolaus.FileName ( fn2fp )
import Data.List ( intersperse )
import Iolaus.Patch.Patchy ( Apply, apply )
import Iolaus.Patch.Commute ()
import Iolaus.Patch.Core ( Named(..) )
import Iolaus.Patch.Prim ( Prim(..),
                         DirPatchType(..), FilePatchType(..) )
import Iolaus.SlurpDirectory ( Slurpy, withSlurpy )
import Iolaus.IO ( WriteableDirectory(..) )
--import Iolaus.FilePathMonad ( withFilePaths, withSubPaths )
#include "impossible.h"
import Iolaus.Ordered ( FL(..), (:>)(..), mapFL_FL, spanFL, unsafeCoerceS )
\end{code}



\section{Introduction}

A patch describes a change to the tree.  It could be either a primitive
patch (such as a file add/remove, a directory rename, or a hunk replacement
within a file), or a composite patch describing many such changes.  Every
patch type must satisfy the conditions described in this appendix.  The
theory of patches is independent of the data which the patches manipulate,
which is what makes it both powerful and useful, as it provides a framework
upon which one can build a revision control system in a sane manner.

Although in a sense, the defining property of any patch is that it can be
applied to a certain tree, and thus make a certain change, this change does
not wholly define the patch.  A patch is defined by a
\emph{representation}, together with a set of rules for how it behaves
(which it has in common with its patch type).  The \emph{representation} of
a patch defines what change that particular patch makes, and must be
defined in the context of a specific tree.  The theory of patches is a
theory of the many ways one can change the representation of a patch to
place it in the context of a different tree.  The patch itself is not
changed, since it describes a single change, which must be the same
regardless of its representation\footnote{For those comfortable with
quantum mechanics, think of a patch as a quantum mechanical operator, and
the representation as the basis set.  The analogy breaks down pretty
quickly, however, since an operator could be described in any complete
basis set, while a patch modifying the file {\tt foo} can only be described
in the rather small set of contexts which have a file {\tt foo} to be
modified.}.

So how does one define a tree, or the context of a patch? The simplest way
to define a tree is as the result of a series of patches applied to the
empty tree\footnote{This is very similar to the second-quantized picture,
in which any state is seen as the result of a number of creation operators
acting on the vacuum, and provides a similar set of simplifications---in
particular, the exclusion principle is very elegantly enforced by the
properties of the anti-hermitian fermion creation operators.}.  Thus, the
context of a patch consists of the set of patches that precede it.

\section{Applying patches}


\begin{code}
--apply_to_filepaths :: Apply p => p C(x y) -> [FilePath] -> [FilePath]
--apply_to_filepaths pa fs = withFilePaths fs (apply pa)

--apply_to_subpaths :: Apply p => p C(x y) -> [SubPath] -> [SubPath]
--apply_to_subpaths pa fs = withSubPaths fs (apply pa)

apply_to_slurpy :: (Apply p, Monad m) => p C(x y)
                -> Slurpy C(x) -> m (Slurpy C(y))
apply_to_slurpy p s = case withSlurpy s (apply p) of
                          Left err -> fail err
                          Right (s', ()) -> return $ unsafeCoerceS s'
\end{code}

\begin{code}
instance Apply p => Apply (Named n p) where
    apply (NamedP _ p) = apply p

instance Apply Prim where
    apply Identity = return ()
    apply (FP f RmFile) = mRemoveFile f
    apply (FP f AddFile) = mCreateFile f
    apply (FP f (Chmod x)) = mSetFileExecutable f x
    apply p@(FP _ (Hunk _ _ _)) = applyFL (p :>: NilFL)
    apply (DP d AddDir) = mCreateDirectory d
    apply (DP d RmDir) = mRemoveDirectory d
    apply (Move f f') = mRename f f'

applyFL :: WriteableDirectory m => FL Prim C(x y) -> m ()
applyFL NilFL = return ()
applyFL ((FP f h@(Hunk _ _ _)):>:the_ps)
 = case spanFL f_hunk the_ps of
       (xs :> ps') ->
           do let foo = h :>: mapFL_FL (\(FP _ h') -> h') xs
              mModifyFilePS f $ hunkmod foo
              applyFL ps'
    where f_hunk (FP f' (Hunk _ _ _)) | f == f' = True
          f_hunk _ = False
          hunkmod :: WriteableDirectory m => FL FilePatchType C(x y)
                  -> B.ByteString -> m B.ByteString
          hunkmod NilFL ps = return ps
          hunkmod (Hunk line old new:>:hs) ps
           = case applyHunkLines [(line,old,new)] ps of
                 Just ps' -> hunkmod hs ps'
                 Nothing -> fail $ "Error applying hunk to file " ++ fn2fp f
          hunkmod _ _ = impossible
applyFL (p:>:ps) = apply p >> applyFL ps
\end{code}

\subsection{Hunk patches}

Hunks are an example of a complex filepatch.  A hunk is a set of lines of a
text file to be replaced by a different set of lines.  Either of these sets
may be empty, which would mean a deletion or insertion of lines.
\begin{code}
applyHunks :: [(Int, [B.ByteString], [B.ByteString])]
           -> B.ByteString -> Maybe [B.ByteString]
applyHunks [] ps = Just [ps]
applyHunks ((l, [], n):hs) ps
    = case break_before_nth_newline (l - 2) ps of
      (prfix, after_prefix) -> do rest <- applyHunks hs after_prefix
                                  return $ intersperse nl (prfix:n) ++ rest
                                       where nl = BC.singleton '\n'
applyHunks ((l, o, n):hs) ps
    = case break_before_nth_newline (l - 2) ps of
      (prfix, after_prefix) ->
          case break_before_nth_newline (length o) after_prefix of
          (oo, _) | oo /= unlinesPS (B.empty:o) -> fail "applyHunks error"
          (_, suffix) ->
              do rest <- applyHunks hs suffix
                 return $ intersperse nl (prfix:n) ++ rest
    where nl = BC.singleton '\n'

applyHunkLines :: [(Int, [B.ByteString], [B.ByteString])]
               -> B.ByteString -> Maybe B.ByteString
applyHunkLines [] c = Just c
applyHunkLines [(1, [], n)] ps | B.null ps = Just $ unlinesPS (n++[B.empty])
applyHunkLines hs@((l, o, n):hs') ps =
 do pss <- case l of
           1 -> case break_after_nth_newline (length o) ps of
                Nothing -> if ps == unlinesPS o
                           then return $ intersperse nl n
                           else fail "applyHunkLines: Unexpected hunks"
                Just (shouldbeo, suffix)
                    | shouldbeo /= unlinesPS (o++[B.empty]) ->
                        fail $ "applyHunkLines: Bad patch!"
                    | null n ->
                        do x <- applyHunkLines hs' suffix
                           return [x]
                    | otherwise ->
                        do rest <- applyHunks hs' suffix
                           return $ intersperse nl n ++ nl:rest
           _ | l < 0 -> bug "Prim.applyHunkLines: After -ve lines?"
             | otherwise -> applyHunks hs ps
    let result = B.concat pss
    return result
    where nl = BC.singleton '\n'
\end{code}

