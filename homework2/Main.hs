{-# OPTIONS_GHC -Wall #-}

module Main where

import Log
import LogAnalysis

main :: IO ()
main = do
  let msg = "E 12 1000 asdfasdf asdfasdf"
  print $ parseMessage msg
  print $ parseMessage "E 2 562 help help"
  print $ parseMessage "I 29 la la la"
  print $ parseMessage "This is not in the right format"

  logMessages <- testParse parse 10 "sample.log"
  print logMessages
  print "Kupa"

  print $ whatWentWrong $ inOrder $ build logMessages
