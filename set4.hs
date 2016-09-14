{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where


import MCPrelude

-- generalA :: (a -> b) -> Gen a -> Gen b
-- transMaybe :: (a -> b) -> Maybe a -> Maybe b
-- general_trans :: (a -> b) -> m a -> m b

-- generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
-- yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- general_yLink :: (a -> b -> c) -> m a -> m b -> m c

-- genTwo :: Gen a -> (a -> Gen b) -> Gen b
-- link :: Maybe a -> (a -> Maybe b) -> Maybe b
-- general_link :: m a -> (a -> m b) -> m b

-- mkGen :: a -> Gen a
-- mkMaybe :: a -> Maybe a
-- general_mkM :: a -> m a

-- Gen
newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }


-- Maybe
data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show (Just a) = "Just " ++ show a
  show Nothing = "Nothing"

-- Card
data Card = Card Int String

instance Show (Card) where
  show (Card x s) = show x ++ s


-- Monad instances

class Monad m where
  bind :: m a -> (a -> m b) -> m b

  (>>=) :: m a -> (a -> m b) -> m b
  (>>=) = bind
  
  return :: a -> m a

instance Monad Maybe where
  bind Nothing _ = Nothing
  bind (Just a) f = f a

  return a = Just a


instance Monad [] where
  bind [] _ = []
  bind xs f = concat (map f xs)

  return a = [a]


instance Monad Gen where
  return a = Gen (\s -> (a, s))

  bind ga f = Gen (\s ->
                     let (a, s2) = runGen ga s
                     in  runGen (f a) s2)



-- functions on Monads

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (m:ms) = return (:) `ap` m `ap` sequence ms


liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = return f `ap` m

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m m2 = return f `ap` m `ap` m2

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

join :: Monad m => m (m a) -> m a
join mma = mma >>= id

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma =
  mf >>= \f ->
  ma >>= \a ->
  return (f a)

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) f g a =
  f a >>= \b -> g b

--- Set 1 Redo

randInt :: Gen Integer
randInt = Gen rand

randLetter :: Gen Char
randLetter = liftM toLetter randInt

randString3 :: String
randString3 = str
  where (str, _) = runGen strGen s
        strGen = sequence (replicate 3 randLetter)
        s = mkSeed(1)

fiveRands :: [Integer]
fiveRands = xs
  where (xs, _) = runGen xsGen s
        xsGen = sequence (replicate 5 randInt)
        s = mkSeed(1)

randEven :: Gen Integer -- the output of rand * 2
randEven = liftM (* 2) randInt

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = liftM (+ 1) randEven

randTen :: Gen Integer -- the output of rand * 10
randTen = liftM (* 10) randInt

randPair :: Gen (Char, Integer)
randPair = return (,) `ap` randLetter `ap` randInt


--- Set 2 redo
headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs

splitMay :: [a] -> Maybe (a, [a])
splitMay l = return (,) `ap` headMay l `ap` tailMay l

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay x ((x2,y):_) | x == x2 = Just y
lookupMay x (_:tail) = lookupMay x tail

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just (x / y)
  
maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just (foldr max x xs)
      
minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = Just (foldr min x xs)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd s =
  (lookupMay s gd) >>= \xs ->
  (splitMay xs) >>= \ht ->
  (maximumMay (snd ht)) >>= \m ->
  divMay (fromIntegral m) (fromIntegral (fst ht))

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries ss n1 n2 = return (+) `ap` lookupMay n1 ss `ap` lookupMay n2 ss

tailProd :: Num a => [a] -> Maybe a
tailProd = tailMay >=> (return . product)

tailSum :: Num a => [a] -> Maybe a
tailSum = tailMay >=> (return . sum)

tailMax :: Ord a => [a] -> Maybe a
tailMax = tailMay >=> maximumMay

tailMin :: Ord a => [a] -> Maybe a
tailMin = tailMay >=> minimumMay

--- Set 3 redo

allPairs :: [a] -> [b] -> [(a,b)]
allPairs = allCombs (,)

allCards :: [Int] -> [String] -> [Card]
allCards = allCombs Card

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f as bs =
  return f `ap` as `ap` bs

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f as bs cs =
  return f `ap` as `ap` bs `ap` cs

