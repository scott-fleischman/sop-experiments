{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Coerce where

import Generics.SOP

ex1 :: NP I '[Maybe Ordering, Maybe Char, Maybe Bool]
ex1 = I (Just LT) :* I (Just 'x') :* I (Just True) :* Nil

ex1' :: NP Maybe '[Ordering, Char, Bool]
ex1' = hcoerce ex1

ex1'' :: NP Maybe '[Ordering, Char, Bool]
ex1'' = Just LT :* Just 'x' :* Just True :* Nil


ex2 :: SOP (K Bool) ('[x1, x2] : xs)
ex2 = SOP (Z (K True :* K False :* Nil))

ex2' :: SOP I '[ '[Bool, Bool], '[Bool] ]
ex2' = hcoerce ex2

ex2'' :: SOP I '[ '[Bool, Bool], '[Bool] ]
ex2'' = SOP (Z (I True :* I False :* Nil))


newtype T1 = T1 ()
t1 :: T1
t1 = T1 ()

newtype T2 = T2 ()
t2 :: T2
t2 = T2 ()

newtype T3 = T3 ()
t3 :: T3
t3 = T3 ()

ex3 :: NP I '[T1, T2, T3]
ex3 = I t1 :* I t2 :* I t3 :* Nil

ex3' :: NP I '[T3, T2, T1]
ex3' = hcoerce ex3
