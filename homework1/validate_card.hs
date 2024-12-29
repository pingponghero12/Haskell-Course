toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x : (y : zs)) = x : (2 * y) : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum $ toDigits x
sumDigits (x : yz) = sum (toDigits x ++ [sumDigits yz])

validate :: Integer -> Bool
validate n
  | sumDigits (doubleEveryOther (toDigitsRev n)) `mod` 10 == 0 = True
  | otherwise = False

main :: IO ()
main = do
  print (toDigits 1234)
  print (toDigitsRev 1234)
  print $ sumDigits $ reverse $ doubleEveryOther $ toDigitsRev 42
  print $ validate 4012888888881881
  print $ validate 4012888888881882
  print $ validate 42
