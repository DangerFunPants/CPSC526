#!/usr/bin/env runhaskell

import Decode
import Data.Bits
import Data.Char
import Text.Show.Pretty (ppShow)

findAll k [] = []
findAll k ls = (zipWith xor k ls) : (findAll k (tail ls))

allAlpha [] = True
allAlpha (x:xs) = 
    if isAlpha x
        then allAlpha xs
        else False

main = do
    d1 <- datastream "transmission4.dat"
    d2 <- datastream "transmission5.dat"
    text' <- datastream "test.txt"
    restText' <- datastream "test2.txt"
    let xord = fmap fromIntegral $ zipWith xor d1 d2
        keyword = fmap ord " the "
        vs = zip (fmap (fmap chr) (findAll keyword xord)) [0..]
        actual = filter (allAlpha . fst) vs
        text = fmap fromIntegral text'
        restText = fmap fromIntegral restText'
        decode = fmap chr $ zipWith xor (drop 2616 xord) text
        rest = take 2616 xord
        decode2 = fmap chr $ reverse $ zipWith xor (reverse rest) (reverse restText)
        entire = decode2 ++ decode
        otherDec = fmap ord entire
        other = fmap chr $ zipWith xor otherDec xord


    putStrLn $ entire

