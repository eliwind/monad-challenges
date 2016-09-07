{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs = allCombs (,)

data Card = Card Int String

instance Show (Card) where
  show (Card x s) = show x ++ s

allCards :: [Int] -> [String] -> [Card]
allCards = allCombs Card

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f as bs =
  combStep (map f as) bs

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f as bs cs =
  combStep (combStep (map f as) bs) cs
  
combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep (f:fs) as =
  map f as ++ combStep fs as
