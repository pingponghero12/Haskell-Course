coPrime :: Int -> Int -> Bool
coPrime a b = gcd a b == 1

phi :: Int -> Maybe Int
phi 1 = Just 1
phi n
  | n < 0 = Nothing
  | otherwise = Just $ length [k | k <- [1 .. n], coPrime k n]

phiJust :: Int -> Int
phiJust 1 = 1
phiJust n = length [k | k <- [1 .. n], coPrime k n]

divisors :: Int -> Maybe [Int]
divisors n
  | n <= 0 = Nothing
  | otherwise = Just [k | k <- [1 .. n], n `mod` k == 0]

sumOfAll :: Int -> Maybe Int
sumOfAll n = case divisors n of
  Nothing -> Nothing
  Just ds -> Just (sum (map phiJust ds))

isIdeal :: Int -> Bool
isIdeal n = sum [d | d <- [1 .. n - 1], n `mod` d == 0] == n

listIdeal :: Int -> [Int]
listIdeal n = filter isIdeal [1 .. n]

divisorsNew :: Int -> [Int]
divisorsNew n
  | otherwise = [k | k <- [1 .. n - 1], n `mod` k == 0]

sumOfDivs :: Int -> Int
sumOfDivs n = sum (divisorsNew n)

allFriends :: Int -> [(Int, Int)]
allFriends n = [(a, b) | a <- [1 .. n], let b = sumOfDivs a, a /= b, b <= n, sumOfDivs b == a, a < b]

dcp :: Int -> Double
dcp n = fromIntegral (length [(k, l) | k <- [2 .. n], l <- [1 .. n], gcd k l == 1]) / fromIntegral (n ^ 2)

-- 6/pi^2
