{-# LANGUAGE CPP #-}
#include "gadts.h"
module Iolaus.Patch.Merge ( mergeN ) where

import Iolaus.Patch.Patchy ( invert, commuteRL, commuteFL )
import Iolaus.Patch.Prim ( Prim )
import Iolaus.Patch.Permutations ( commuteWhatWeCanFL, removeFL )
import Iolaus.Ordered ( EqCheck(..), (=\/=), FL(..), (:>)(..),
                        reverseFL, reverseRL )
import Iolaus.Sealed ( Sealed(..), mapSeal )
#include "impossible.h"

mergeN :: [Sealed (FL Prim C(x))] -> Sealed (FL Prim C(x))
mergeN [] = error "mergeN on []"
mergeN [x] = x
mergeN (Sealed NilFL : r) = mergeN r
mergeN xsr@(Sealed (x:>:xs) : r) =
    case mapM (merge1 x) r of
      Just r' -> (x :>:) `mapSeal` mergeN (Sealed xs:r')
      Nothing -> mergeN $ map (kill1 x) xsr

kill1 :: Prim C(x y) -> Sealed (FL Prim C(x)) -> Sealed (FL Prim C(x))
kill1 a (Sealed xs) =
    case commuteWhatWeCanFL (invert a :> xs) of
      xs' :> a' :> _ ->
          case commuteRL (reverseFL xs' :> a') of
            Just (ia :> xs'') ->
                case ia =\/= invert a of
                  IsEq -> Sealed (reverseRL xs'')
                  NotEq -> impossible
            _ -> impossible

merge1 :: Prim C(x y) -> Sealed (FL Prim C(x)) -> Maybe (Sealed (FL Prim C(y)))
merge1 a (Sealed bs) =
    case removeFL a bs of
      Just bs' -> Just $ Sealed bs'
      Nothing -> do bs' :> _ <- commuteFL (invert a :> bs)
                    Just $ Sealed bs'
