module RecordBase where

import Extensible


extensible [d|
    data Rec
      = R1 { beep :: Bool, boop :: Int }
      | R2 { inner :: Rec }
  |]
