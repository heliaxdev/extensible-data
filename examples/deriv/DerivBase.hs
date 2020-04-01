module DerivBase where

import Extensible

extensible [d|
    data A a = A (B a) Int deriving Eq
    data B a = B a deriving Eq
  |]
