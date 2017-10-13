{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Fluid where

import Generics.SOP
import Generics.SOP.Lens

data T0 = T0
data T1 = T1
data T2 = T2
data T3 = T3

data S0 = S0
data S1 = S1
data S2 = S2

valueT :: NS I '[T0, T1, T2, T3]
valueT = S (S (S (Z (I T3))))

transform :: NS I (T0 ': T1 ': ts) -> NS I (S0 ': S1 ': S2 ': ts)
transform (Z (I T0))      = (S (S (Z (I S2))))
transform (S (Z (I T1)))  = (S (S (Z (I S2))))
transform (S (S tn))      = (S (S (S tn)))


pattern NS0 x = Z x

pattern RS1 x = S x
pattern RS2 x = S (RS1 x)
pattern RS3 x = S (RS2 x)

pattern NS1 x = RS1 (NS0 x)
pattern NS2 x = RS2 (NS0 x)
pattern NS3 x = RS3 (NS0 x)

transform' :: NS I (T0 ': T1 ': ts) -> NS I (S0 ': S1 ': S2 ': ts)
transform' (NS0 (I T0))  = (NS2 (I S2))
transform' (NS1 (I T1))  = (NS2 (I S2))
transform' (RS2 tn)      = (RS3 tn)
