{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage ('E' : xs) =
  let parts = words xs
   in let err = read (head parts) :: Int
          ts = read (parts !! 1) :: Int
          text = unwords (drop 2 parts)
       in LogMessage (Error err) ts text
parseMessage ('I' : xs) =
  let parts = words xs
   in let ts = read (head parts) :: Int
          text = unwords (drop 1 parts)
       in LogMessage Info ts text
parseMessage ('W' : xs) =
  let parts = words xs
   in let ts = read (head parts) :: Int
          text = unwords (drop 1 parts)
       in LogMessage Warning ts text
parseMessage n = Unknown n

parseHelper :: [String] -> [LogMessage]
parseHelper [] = []
parseHelper (x : xs) = parseMessage x : parseHelper xs

parse :: String -> [LogMessage]
parse n =
  let lns = lines n
   in parseHelper lns

insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ n _) (Node left log@(LogMessage _ m _) right)
  | n < m = Node (insert msg left) log right
  | otherwise = Node left log (insert msg right)
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x : xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error lev) _ text) : xs)
  | lev >= 50 = text : whatWentWrong xs
  | otherwise = whatWentWrong xs
whatWentWrong (_ : xs) = whatWentWrong xs
