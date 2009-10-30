{-# LANGUAGE CPP #-}
#include "gadts.h"
module Iolaus.Patch.Merge ( mergeNamed ) where

import Iolaus.Patch.Patchy ( Patchy, invert, commuteRL, commuteFL, commuteRLFL,
                             identity )
import Iolaus.Patch.Prim ( Prim(..), FilePatchType(Chunk), Effect, splatter )
import Iolaus.Patch.Core ( Named(NamedP) )
import Iolaus.Patch.Permutations ( commuteWhatWeCanFL, removeFL )
import Iolaus.Patch.Apply ()
import Iolaus.Patch.Viewing ()
import Iolaus.Ordered ( EqCheck(..), (=\/=), FL(..), (:>)(..),
                        reverseFL, reverseRL )
import Iolaus.Sealed ( Sealed(..), mapSeal )

import qualified Data.ByteString.Char8 as BC ( pack )

#include "impossible.h"

instance Patchy Prim
instance (Show a, Patchy p, Effect p) => Patchy (Named a p)

mergeNamed :: FORALL(x) [Sealed (FL (Named String Prim) C(x))]
           -> Sealed (FL (Named String Prim) C(x))
mergeNamed [] = Sealed NilFL
mergeNamed [x] = x
mergeNamed (Sealed NilFL : r) = mergeNamed r
mergeNamed xsr@(Sealed (x:>:xs) : r) =
    case mapM (merge1 x) r of
      Just r' -> (x :>:) `mapSeal` mergeNamed (Sealed xs:r')
      Nothing -> mergeNamed $ marked : map (kill1 x) xsr
    where xxx :: [Sealed (Named String Prim C(x))]
          xxx = concatMap (splatters . conflicting x) xsr
          splatters (Sealed a) = case splatterNamed a of
                                   Just b -> [Sealed b]
                                   Nothing -> []
          bigid = mkIdentity xxx
          yyy = map mkbig xxx
          mkbig (Sealed z) = case splatterNamed (bigid :>: z:>:NilFL) of
                               Just b ->  Sealed b
                               Nothing -> Sealed z
          marked = markConflict yyy

markConflict :: [Sealed (Named String Prim C(x))]
             -> Sealed (FL (Named String Prim) C(x))
markConflict [Sealed x] = Sealed $ x:>:NilFL -- puzzling possibility
markConflict zzz@(Sealed (NamedP _ (FP f (Chunk c w o _))) : _) =
    Sealed $ NamedP "merge" (FP f (Chunk c w o markedns)):>: NilFL
    where getn (Sealed (NamedP a (FP _ (Chunk _ _ _ n)))) = [(a,n)]
          getn _ = []
          ns = concatMap getn zzz
          markn (a,n) = [BC.pack ("|||"++a++">>>"),BC.pack "\n"]++n++
                        [BC.pack "\n",BC.pack ("<<<"++a++"|||"),BC.pack "\n"]
          markedns = concatMap markn ns
markConflict _ = Sealed NilFL

mkIdentity :: [Sealed (Named String Prim C(x))]
             -> Named String Prim C(x x)
mkIdentity xs = case splatterNamed $ mkFL xs of
                  Just a -> a
                  Nothing -> error ("xxxxxx\n"++show (length xs))
                            --  NamedP "identity" identity
    where mkFL (Sealed z:zs) = z :>: invert z :>: mkFL zs
          mkFL [] = NilFL

splatterNamed :: FL (Named String Prim) C(x y)
              -> Maybe ((Named String Prim) C(x y))
splatterNamed (NamedP _ x :>: NamedP b y :>: c) =
    do xy <- splatter (x :> y)
       splatterNamed (NamedP b xy :>: c)
splatterNamed (NamedP a x :>: NilFL) = Just $ NamedP a x
splatterNamed NilFL = Just $ NamedP "identity" identity

conflicting :: Patchy p => p C(x y) -> Sealed (FL p C(x))
            -> Sealed (FL p C(x))
conflicting a (Sealed xs) =
    case commuteWhatWeCanFL (invert a :> xs) of
      xs' :> a' :> ys ->
          case commuteRL (reverseFL xs' :> a') of
            Just (ia :> xs'') ->
                case ia =\/= invert a of
                  NotEq -> impossible
                  IsEq -> case commuteRLFL (xs'' :> ys) of
                          Just (ys' :> _) -> Sealed ys'
                          Nothing -> impossible
            _ -> impossible

kill1 :: Patchy p => p C(x y) -> Sealed (FL p C(x)) -> Sealed (FL p C(x))
kill1 a (Sealed xs) =
    case commuteWhatWeCanFL (invert a :> xs) of
      xs' :> a' :> _ ->
          case commuteRL (reverseFL xs' :> a') of
            Just (ia :> xs'') ->
                case ia =\/= invert a of
                  IsEq -> Sealed (reverseRL xs'')
                  NotEq -> impossible
            _ -> impossible

merge1 :: Patchy p => p C(x y) -> Sealed (FL p C(x))
       -> Maybe (Sealed (FL p C(y)))
merge1 a (Sealed bs) =
    case removeFL a bs of
      Just bs' -> Just $ Sealed bs'
      Nothing -> do bs' :> _ <- commuteFL (invert a :> bs)
                    Just $ Sealed bs'
