{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

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
maximumMay (x:xs) = Just (maximum x xs)
  where
    maximum m [] = m
    maximum m (x:xs) = maximum (max m x) xs
      
minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = Just (minimum x xs)
  where
    minimum m [] = m
    minimum m (x:xs) = minimum (min m x) xs


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
queryGreek2 gd =
  \s -> link (lookupMay s gd)
  (\xs -> link (tailMay xs)
    (\t -> link (maximumMay t)
      (\m -> link (headMay xs)
        (\h -> divMay (fromIntegral m) (fromIntegral h)))))
                           

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries ss n1 n2 =
  link (lookupMay n1 ss)
  (\s1 -> link (lookupMay n2 ss)
    (\s2 -> mkMaybe (s1 + s2)))

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f ma mb =
  link ma
    (\x -> link mb
      (\y -> mkMaybe (f x y)))

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 ss n1 n2 = yLink (+) (lookupMay n1 ss) (lookupMay n2 ss)

mkMaybe :: a -> Maybe a
mkMaybe a = Just a

tailProd :: Num a => [a] -> Maybe a
tailProd xs = transMaybe product (tailMay xs)

tailSum :: Num a => [a] -> Maybe a
tailSum xs = transMaybe sum (tailMay xs)

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f mx = link mx (mkMaybe . f)
  
tailMax :: Ord a => [a] -> Maybe a
tailMax xs = combine (transMaybe maximumMay (tailMay xs))

tailMin :: Ord a => [a] -> Maybe a
tailMin xs = combine (transMaybe minimumMay (tailMay xs))

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing = Nothing
combine (Just x) = x
