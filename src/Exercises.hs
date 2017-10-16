{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Exercises where
-- https://github.com/kosmikus/sop-haskellx-2017/blob/606a27c1e4b3f5c4573d234688eb95f1283072e3/src/Exercises.hs

import Generics.SOP
import Generics.SOP.NP
import Generics.SOP.GGP
import qualified Generics.SOP.Type.Metadata as T
import qualified Generics.SOP.Constraint as C
import qualified GHC.Generics as GHC
import GHC.TypeLits

data Cucumber = Cucumber Int Char Bool deriving (Show, GHC.Generic)
instance Generic Cucumber
instance HasDatatypeInfo Cucumber

data Nut a = Walnut Int | Chestnut Int a Int deriving (Show, GHC.Generic)
instance Generic (Nut a)
instance HasDatatypeInfo (Nut a)

data MyProd = MyProd Int Double deriving (Show, GHC.Generic)
instance Generic MyProd
instance HasDatatypeInfo MyProd

productFrom :: (IsProductType a xs) => a -> NP I xs
productFrom = unZ . unSOP . from

productTo   :: (IsProductType a xs) => NP I xs -> a
productTo = to . SOP . Z

emptyProduct :: (IsProductType a xs, All Monoid xs) => a
emptyProduct = productTo $ hcpure (Proxy @Monoid) (I mempty)

appendProduct :: (IsProductType a xs, All Monoid xs) => a -> a -> a
appendProduct a1 a2 = productTo $ hcliftA2 (Proxy @Monoid) (mapIII mappend) (productFrom a1) (productFrom a2)

zeroProduct :: (IsProductType a xs, All Num xs) => a
zeroProduct = productTo $ hcpure (Proxy @Num) (I 0)

incrementProduct :: (IsProductType a xs, All Num xs) => a -> a
incrementProduct a = productTo $ hcliftA (Proxy @Num) (mapII (+1)) (productFrom a)
