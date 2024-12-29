type Peg = String

type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n - 1) c b a ++ [(a, b)] ++ hanoi (n - 1) a c b

nagoi :: Int -> Peg -> Peg -> Peg -> Peg -> [Move]
nagoi 1 a b c d = [(a, b)]
nagoi 2 a b c d = hanoi 2 a b c
nagoi n a b c d = [(d, b)] ++ hanoi (n - 2) c b a ++ [(a, b)] ++ hanoi (n - 2) a c b ++ [(a, d)]

main :: IO ()
main = do
  print $ hanoi 2 "a" "b" "c"
  print $ nagoi 2 "a" "b" "c" "d"
  print $ nagoi 3 "a" "b" "c" "d"
  print $ nagoi 4 "a" "b" "c" "d"
  print $ length $ nagoi 15 "a" "b" "c" "d"
