module MultiFieldBase where
import Extensible

extensible [d| data A a = A a | B (A a) (A Int) |]
