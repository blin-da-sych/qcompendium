{-# LANGUAGE ScopedTypeVariables #-}
module Operations where

import           BasisStrict (Basis, PA, QV, amplitude, basis, qVector)
import           Data.Map    (Map)
import           Pairs       ()

newtype Qop a b =
  Qop (Map (a, b) PA)

qop :: (Basis a, Basis b) => [((a, b), PA)] -> Qop a b
qop = Qop . qVector

qNot :: QV Bool -> QV Bool
qNot v = qVector [(False, amplitude v True), (True, amplitude v False)]

hadamard :: QV Bool -> QV Bool
hadamard v =
  let a = amplitude v False
      b = amplitude v True
   in qVector [(False, a + b), (True, a - b), (False, a + b)]

qApp :: (Basis b, Basis a) => Qop a b -> QV a -> QV b
qApp (Qop m) v =
  let bF b = sum [amplitude m (a, b) * amplitude v a | a <- basis]
   in qVector [(b, bF b) | b <- basis]
