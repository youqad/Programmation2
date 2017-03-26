-- import Data.Complex

-- bind :: (Complex Float -> [Complex Float]) -> ([Complex Float] -> [Complex Float])

-- bind f l = concat (map f l)

-- return x = [x]

-- lift f = return . f

import System.Random


bind :: (a -> StdGen -> (b, StdGen)) -> (StdGen -> (a, StdGen)) -> (StdGen -> (b, StdGen))
bind f x seed = let (a', seed') = x seed in f a' seed'

return a seed = (a, seed)

lift f = return . f


