{-# LANGUAGE DataKinds #-}

module Basic where

import Generics.SOP
import Control.Lens
import Generics.SOP.Lens

myNP :: NP I '[ Char, Bool ]
myNP = I 'q' :* I False :* Nil

myNS :: NS I '[ Char, Bool ]
myNS = Z (I 'b')

myNS2 :: NS I '[ Char, Bool ]
myNS2 = S (Z (I True))

myNS_NP :: NS I '[ (NP I '[Char, Bool]), (NP I '[Int, Double]) ]
myNS_NP = S (Z (I (I 42 :* I 3.14 :* Nil)))

mySOP :: SOP I '[ '[Char, Bool], '[Int, Double] ]
mySOP = SOP (Z (I 'q' :* I False :* Nil))

charToString :: Char -> String
charToString c = replicate 5 c

npFn
  :: NP I '[Char,   Bool]
  -> NP I '[String, Bool]
npFn = over (headLens . uni) charToString

nsFn
  :: NS I '[Char,   Bool]
  -> NS I '[String, Bool]
nsFn = over (_Z . uni) charToString

ns_npFn
  :: NS I '[ (NP I '[Char,   Bool]), (NP I '[Int, Double]) ]
  -> NS I '[ (NP I '[String, Bool]), (NP I '[Int, Double]) ]
ns_npFn = over (_Z . uni . headLens . uni) charToString

ns_np2Fn
  :: NS (NP I) '[ '[Char,   Bool], '[Int, Double] ]
  -> NS (NP I) '[ '[String, Bool], '[Int, Double] ]
ns_np2Fn = over (_Z . headLens . uni) charToString

sopFn
  :: SOP I '[ '[Char,   Bool], '[Int, Double] ]
  -> SOP I '[ '[String, Bool], '[Int, Double] ]
sopFn = over (unsop . _Z . headLens . uni) charToString

sopAFN :: Applicative m
  => (Int -> m Char)
  ->    SOP I '[ '[Char, Bool], '[Int,  Double] ]
  -> m (SOP I '[ '[Char, Bool], '[Char, Double] ])
sopAFN = unsop . _S . _Z . headLens . uni
