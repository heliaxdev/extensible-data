module MutualBase where
import Extensible

extensible [d|
    data A = AA A | AB B B
    data B = BA A | BZ
  |]
