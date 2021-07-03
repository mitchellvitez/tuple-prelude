{-# LANGUAGE TemplateHaskell #-}

module Main where

import Helpers
import GHC.Tuple (Unit(..))

$(rangeOverTuples 1 "head" headN)
$(rangeOverTuples 1 "tail" tailN)
$(rangeOverTuples 1 "init" initN)
$(rangeOverTuples 1 "last" lastN)
$(rangeOverTuples 0 "length" lengthN)
$(rangeOverTuples 0 "null" nullN)
$(rangeOverTuples 0 "map" mapN)

main :: IO ()
main = do
  putStrLn $ "Max tuple size: " <> show maxTupleSize

  example 1 $ names 1 10
  example 2 $ namedTupleP 7

  example 3 $ head5 ("yes", "no", "no", "no", "no")
  example 4 $ tail4 (1, 2, 3, 4)
  example 5 $ init3 ("test", 'b', 5)
  example 6 $ last2 ((1, 2), (3, 4))

  example 7 $ length5 (1, 2, 3, 4, 5)

  example 8 $ null0 ()
  example 9 $ null1 (Unit "one")
  example 10 $ null2 ('a', 'b')

  example 11 $ map5 (+1) (1, 2, 3, 4, 5)
  example 12 $ map3 length (['0'..'9'], "looooooooooooooooong", ['a'..'z'])

example :: Show a => Int -> a -> IO ()
example n ex = putStrLn $ show n ++ ". " ++ show ex
