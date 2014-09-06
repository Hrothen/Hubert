{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (sequence_)

import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import HTML.Parser
import Dom

test1 :: IO ()
test1 = readFile "test.html" >>= return . parseHtml . T.pack >>= print

tests = [test1]

testsVerbose = zipWith (\t i -> putStrLn ("Starting Test " ++ show i) >>
                                t >> putStrLn ("Test " ++ show i ++ " complete"))
                        tests [1..(length tests)]

main :: IO ()
main = sequence_ testsVerbose