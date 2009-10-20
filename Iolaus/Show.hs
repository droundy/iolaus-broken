{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP #-}

module Iolaus.Show(Show1(..), Show2(..), Eq1(..), Ord1(..),
                   Pretty(..), Pretty1(..),
                   showOp2, app_prec) where

#include "gadts.h"

class Show1 a where
    show1 :: a C(x) -> String
    show1 x = showsPrec1 0 x ""
    showsPrec1 :: Int -> a C(x) -> ShowS
    showsPrec1 _ x s = show1 x ++ s
class Eq1 a where
    eq1 :: a C(x) -> a C(y) -> Bool
class Eq1 a => Ord1 a where
    compare1 :: a C(x) -> a C(y) -> Ordering

class Show2 a where
    show2 :: a C(x y) -> String
    show2 x = showsPrec2 0 x ""
    showsPrec2 :: Int -> a C(x y) -> ShowS
    showsPrec2 _ x s = show2 x ++ s

showOp2 :: (Show2 a, Show2 b) => Int -> String -> Int -> a C(w x) -> b C(y z) -> String -> String
showOp2 prec opstr d x y = showParen (d > prec) $ showsPrec2 (prec + 1) x .
                          showString opstr . showsPrec2 (prec + 1) y

app_prec :: Int
app_prec = 10

class Pretty a where
    pretty :: a -> String
class Pretty1 a where
    pretty1 :: a C(x) -> String
