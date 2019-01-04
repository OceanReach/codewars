{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Codewars.G964.Mixin where

import qualified Data.Map.Strict as MpS
import qualified Data.Foldable as F
import qualified Data.Ord as O
import qualified Data.List as L
import qualified Data.Monoid as Moi

data Lable = One | Two | Both
  deriving (Eq,O.Ord)
instance Show Lable where
  show One = "1:"
  show Two = "2:"
  show Both = "=:"


type MCfreq = MpS.Map Char (Lable, Int)
type LCfreq = [(Lable,Int,Char)]

class TransLCf a where
  trans_lcf :: LCfreq -> a

instance TransLCf String where
  trans_lcf [] = ""
  trans_lcf lcf = F.foldMap makeIt lcf
    where
      makeIt (l,n,c)
        | n > 1 = "/" ++ show l ++ replicate n c
        | otherwise = ""

mcf2lcf :: MCfreq -> LCfreq
mcf2lcf = MpS.foldrWithKey' makeIt []
  where
    makeIt k (l,n) rs = (l,n,k):rs

lcf2String :: LCfreq -> String
lcf2String lcf = let str = trans_lcf lcf in
                   if null str
                   then str
                   else tail str

sort_lcf :: LCfreq -> LCfreq
sort_lcf = L.sortBy cmpare
  where
    cmpare = cmpare1 Moi.<> cmpare2 Moi.<> cmpare3
    cmpare3 = O.comparing get3
    cmpare2 = O.comparing get1
    cmpare1 x y = let n1 = get2 x
                      n2 = get2 y in
                    case compare n1 n2 of
                      GT -> LT
                      LT -> GT
                      EQ -> EQ
    get1 (l,_,_) = l
    get2 (_,n,_) = n
    get3 (_,_,c) = c



make_mcf_noBlank :: Lable -> String -> MCfreq
make_mcf_noBlank lbe = L.foldr makeIt MpS.empty
  where
    makeIt c m
      | c >= 'a' && c <= 'z' = MpS.insertWith (\_ (l,n) -> (l,n+1)) c (lbe, 1) m
      | otherwise = m

mix :: [Char] -> [Char] -> [Char]
mix s1 s2 = lcf2String lc_f
  where
    lc_f = sort_lcf $ mcf2lcf mc_f3
    mc_f3 = MpS.unionWith processIt mc_f1 mc_f2
      where
        processIt (l1,n1) (l2,n2)
          | n1 == n2 = (Both, n1)
          | n1 > n2 = (l1, n1)
          | n1 < n2 = (l2, n2)
          
    mc_f1 = if null s1 then MpS.empty else make_mcf_noBlank One s1
    mc_f2 = if null s2 then MpS.empty else make_mcf_noBlank Two s2


--Test String
s1="Are the kids at home? aaaaa fffff" :: String
s2="Yes they are here! aaaaa fffff" :: String


s3 = "mmmmm m nnnnn y&friend&Paul has heavy hats! &" :: String
s4 = "my frie n d Joh n has ma n y ma n y frie n ds n&" :: String
