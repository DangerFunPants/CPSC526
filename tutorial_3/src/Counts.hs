#!/usr/bin/env runhaskell

module Counts 
    ( frequentWords'
    , frequentWords
    , longestWord
    , occuringN 
    , longestOccuringN
    )
    where

import Data.List (sortOn)
import qualified Data.Map as M
import Text.Show.Pretty (ppShow)

frequentWords'  :: (Ord t, Num a, Enum a) => [t] -> M.Map t a -> M.Map t a
frequentWords' [] d = d
frequentWords' (w:ws) d = frequentWords' ws new
    where
        old = M.findWithDefault 0 w d
        new = M.insert w (succ old) d

frequentWords :: (Ord b, Enum b, Num b) => String -> [(String, b)]
frequentWords s = take 10 $ (reverse . (sortOn snd)) $ M.toList $ frequentWords' (words s) M.empty

longestWord :: String -> String
longestWord = (head . reverse . (sortOn length) . words)

longestOccuringN :: [(String, Int)] -> Int -> [(String, Int)]
longestOccuringN ps n = take 10 $ (reverse . (sortOn (length . fst))) (occuringN ps n)

occuringN :: [(String, Int)] -> Int -> [(String, Int)]
occuringN ps n = [ (w, c) | (w, c) <- ps, c > n ]

main :: IO ()
main = do
    s <- getContents
    let res = frequentWords s
        longest = longestWord s
        longestN = longestOccuringN res 25
    putStrLn $ ppShow res
    putStrLn $ ppShow longest
    putStrLn $ ppShow longestN
