module Example where

import           Data.Map    (singleton)
import           Qcompendium (Move (Vertical), QV, qv)

qFalse :: QV Bool
qFalse = singleton False 1

qTrue :: QV Bool
qTrue = singleton True 1

qFT :: QV Bool
qFT = qv [(False, 1 / sqrt 2), (True, 1 / sqrt 2)]

qUp :: QV Move
qUp = singleton Vertical 1
