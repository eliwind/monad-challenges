{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show (Just a) = "Just " ++ show a
  show Nothing = "Nothing"

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay x [] = Nothing
lookupMay x ((x2,y):_) | x == x2 = Just y
lookupMay x (_:tail) = lookupMay x tail
  
divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay x 0 = Nothing
divMay x y = Just (x / y)

  
maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just (foldl max x xs)
      
minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = Just (foldl min x xs)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd s =
  case lookupMay s gd of
    Nothing -> Nothing
    Just xs ->
      case tailMay xs of
        Nothing -> Nothing
        Just t ->
          case maximumMay t of
            Nothing -> Nothing
            Just m ->
              case headMay xs of
                Nothing -> Nothing
                Just h -> divMay (fromIntegral m) (fromIntegral h)

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain f (Just a) = f a 

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link Nothing _ = Nothing
link (Just a) f = f a 

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd s =
  (lookupMay s gd) `link` \xs ->
  (tailMay xs) `link` \t ->
  (maximumMay t) `link` \m ->
  (headMay xs) `link` \h ->
  divMay (fromIntegral m) (fromIntegral h)
                           

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries ss n1 n2 =
  (lookupMay n1 ss) `link` \s1 ->
  (lookupMay n2 ss) `link` \s2 ->
  mkMaybe (s1 + s2)

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f ma mb =
  ma `link` \x ->
  mb `link` \y ->
  mkMaybe (f x y)

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 ss n1 n2 = yLink (+) (lookupMay n1 ss) (lookupMay n2 ss)

mkMaybe :: a -> Maybe a
mkMaybe a = Just a

tailProd :: Num a => [a] -> Maybe a
tailProd = (transMaybe product) . tailMay

tailSum :: Num a => [a] -> Maybe a
tailSum = (transMaybe sum) . tailMay

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f mx = link mx (mkMaybe . f)
  
tailMax :: Ord a => [a] -> Maybe a
tailMax = combine . (transMaybe maximumMay) . tailMay

tailMin :: Ord a => [a] -> Maybe a
tailMin = combine . (transMaybe minimumMay) . tailMay

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing = Nothing
combine (Just x) = x
