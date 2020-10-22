import Prelude ()
import Stream

f1 = (input :: Stream Int) >>> output

f2 = (input :: Stream Int) >>> foreach (+2) >>> output

f3 = (input :: Stream Int) >>> group 2 0 (+) >>> output

f4 = (input :: Stream Bool) >>> foreach not >>> output

f5 = (input :: Stream Int) >>> group 3 true (\b n -> b && (n < 10)) >>> output

f5' = (input :: Stream Int) >>> foreach (<10) >>> group 3 true (&&) >>> output

f6 = (input :: Stream Int) >>> group 3 0 (+) >>> foreach (==10) >>> output

fconst = (constinput 1 :: Stream Int) >>> output

-- only works with shallow embedding and not with deep as max exists in haskell only
-- fmax = (input :: Stream Int) >>> group 3 0 (max) >>> output

f7' = (input :: Stream Int) >>> group 3 0 (\m c -> if' (c > m) c m) >>> output

f8 = (input :: Stream Int) >>> filter' (>5) >>> output


