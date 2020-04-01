module DerivBase where

import Extensible

extensible [d| data A a = A a Int deriving Eq |]
