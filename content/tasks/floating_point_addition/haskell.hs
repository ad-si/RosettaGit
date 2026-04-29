import Data.Ratio ((%))

main :: IO ()
main = do
  -- Double precision (IEEE 754 binary64)
  let s = 0.1 + 0.2 :: Double
  print s              -- 0.30000000000000004
  print (s == 0.3)     -- False

  -- Exact rationals
  let r = 1 % 10 + 2 % 10 :: Rational
  print r              -- 3 % 10
  print (r == 3 % 10)  -- True
