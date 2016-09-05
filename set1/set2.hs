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
