f :: Int -> Int
f = \x -> (1 + x * (x + 1))

g = \x -> (\y -> (x + y ^ 2))

h = \y -> (\x -> (x + y ^ 2))

ni :: (Int -> Int) -> Int
ni fx = fx 2

nn :: (Int -> Int) -> (Int -> Int)
nn fx = \x -> (1 + fx x)

ng :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
ng fx gx = \x -> (fx (gx x))

ff = \x -> (2 ^)

dupa :: Double -> Double
dupa d = d
