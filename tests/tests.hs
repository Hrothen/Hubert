module Main where

import Control.Monad (sequence_)

import HTML.Parser
import Dom

test1 :: IO ()
test1 = readFile "test.html" >>= return . parseHtml >>= print

tests = [test1]

testsVerbose = zipWith (\t i -> putStrLn ("Starting Test " ++ show i) >>
                                t >> putStrLn ("Test " ++ show i ++ " complete"))
                        tests [1..(length tests)]

main :: IO ()
main = sequence_ testsVerbose