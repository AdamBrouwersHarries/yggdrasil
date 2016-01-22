module Main where

main = putStrLn $ show $ groupUp df [1..100]

df :: Integral a => a -> Bool
df x = x `mod` 5 == 0

groupUp :: (a -> Bool) -> [a] -> [[a]]
groupUp _ [] = [] 
groupUp f (x:xs) = let sp = break f xs in
    (x : fst sp) : (groupUp f (snd sp))