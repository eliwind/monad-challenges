{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude


fiveRands :: [Integer]
fiveRands = [r1, r2, r3, r4, r5]
  where s1 = mkSeed 1
        (r1, s2) = rand s1
        (r2, s3) = rand s2
        (r3, s4) = rand s3
        (r4, s5) = rand s4
        (r5, s6) = rand s5

type Gen a = Seed -> (a, Seed)


randLetter :: Gen Char
randLetter s = ((toLetter x), s2)
  where (x, s2) = rand s

randString3 :: String
randString3 = [r1, r2, r3]
  where s1 = mkSeed 1
        (r1, s2) = randLetter s1
        (r2, s3) = randLetter s2
        (r3, s4) = randLetter s3

generalA :: (a -> b) -> Gen a -> Gen b
generalA f gena s = (f x, s2)
  where (x, s2) = gena s

randEven :: Gen Integer -- the output of rand * 2
randEven = generalA (* 2) rand

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = generalA (+ 1) randEven

randTen :: Gen Integer -- the output of rand * 10
randTen = generalA (* 10) rand

randPair :: Gen (Char, Integer)
randPair s = ((c, n), s3)
  where (c, s2) = randLetter s
        (n, s3) = rand s2

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair gena genb s = ((a, b), s3)
  where (a, s2) = gena s
        (b, s3) = genb s2

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f gena genb s = (f a b, s3)
  where (a, s2) = gena s
        (b, s3) = genb s2
  
generalPair2 :: Gen a -> Gen b -> Gen (a,b)
generalPair2 = generalB (,)

generalB2 :: (a -> b -> c) -> Gen b -> Gen a -> Gen c
generalB2 f genb gena = generalB f gena genb
  
repRandom :: [Gen a] -> Gen [a]
repRandom genas = foldl (generalB2 (:)) (mkGen []) genas

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo gena f s = f a s2
  where (a, s2) = gena s

mkGen :: a -> Gen a
mkGen a s = (a, s)

