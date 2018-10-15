#!/usr/bin/env runhaskell

module Decode 
    ( shiftDistance
    , keystream
    , datastream
    , printDecrypt
    ) where

import Data.Char (ord, chr)
import Text.Show.Pretty (ppShow)
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Bits (xor)
import GHC.Word (Word8)
import Counts
import Data.List (sortOn)
import qualified Data.Set as S

shiftDistance :: [(Word8, Word8)] -> [Word8]
shiftDistance = fmap (\(p, c) ->  c `xor` p) 

decode :: [(Word8, Word8)] -> [Word8]
decode = fmap (\(p, k) -> p `xor` k)

keystream :: String -> [Word8]
keystream k = (concat . repeat) ((fromIntegral . ord) <$> k) :: [Word8]

datastream :: String -> IO [Word8]
datastream fname = do
    t <- B.readFile fname
    return $ B.unpack t

printDecrypt :: String -> String -> IO ()
printDecrypt f keyStr = do
    d <- datastream f
    let dec = decode (zip d k)
        k = keystream keyStr
    putStrLn $ show $ fmap (chr . fromIntegral) dec

analyzeSequences :: (Ord a) => [a] -> Int -> M.Map [a] Int -> M.Map [a] Int
analyzeSequences [] _ d = d
analyzeSequences as n d = analyzeSequences (tail as) n newD
    where
        thisSeq = take n as
        current = M.findWithDefault 0 thisSeq d
        newD = M.insert thisSeq (succ current) d

mkWindows :: [a] -> Int -> [[a]]
mkWindows [] _ = []
mkWindows as n = (take n as) : (mkWindows (tail as) n)

symbolDistance :: (Num b, Enum b, Eq a) => a -> [a] -> [b]
symbolDistance s xs = ds
    where
        is = zip xs [0..]
        vs = filter (\(a, i) -> a == s) is
        ds = fmap (\(t1, t2) -> (snd t2) - (snd t1)) (zip vs (tail vs))

factors :: (Enum a, Eq a, Integral a) => a -> [a]
factors n = [ f | f <- [2..(pred n)], (n `mod` f) == 0 ]

kasiski :: (Eq t, Integral b) => t -> [t] -> S.Set b
kasiski t d = cs
    where
        dist = symbolDistance t d
        fs = fmap factors dist
        s = fmap S.fromList fs
        cs = foldl (\s v -> S.intersection v s) (S.fromList (head fs)) s

divMsg :: Int -> [a] -> [[a]]
divMsg n d = [ [ m | (m, i) <- zip d [0..], (i `mod` n) == x ] | x <- [0..(pred n)] ]

findXor :: Int -> [Int]
findXor v = [ x | x <- [0..255], v `xor` x == (ord 'e') ]

main :: IO () 
main = do
    d <- datastream "transmission3.dat"
    let tripleCounts = (reverse . (sortOn snd) . M.toList) $ analyzeSequences d 3 M.empty
        ts = fmap fst tripleCounts
        threes = mkWindows d 3
        r = fmap ((flip kasiski) threes) ts
        divd = divMsg 5 d
        freqs = fmap (\v -> head $ reverse $ sortOn snd $ M.toList $ frequentWords' v M.empty) divd
        ks = concat $ fmap findXor (fmap (fromIntegral . fst) freqs)
        key = fmap chr ks
    putStrLn $ "The key is: " ++ key
    putStrLn $ "The transmission is: "
    printDecrypt "transmission3.dat" key

