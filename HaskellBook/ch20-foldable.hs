import Data.Monoid
import Data.Semigroup

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = getAny . foldMap (Any . (==x))

minimum :: (Foldable t, Ord a, Bounded a) => t a -> a
minimum = getMin . foldMap Min

maximum :: (Foldable t, Ord a, Bounded a) => t a -> a
maximum = getMax . foldMap Max

null :: (Foldable t) => t a -> Bool
null = not . getAny . foldMap (Any . const True)

length :: (Foldable t) => t a -> Int
length = getSum . foldMap (Sum . const 1)

toList :: Foldable t => t a -> [a]
toList = foldMap (:[])

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap2 :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap2 f = foldr (\x y -> f x <> y) mempty
foldMap3 f = foldr (mappend . f) mempty

main = do
  print $ Main.sum [1..4] == 10
  print $ Main.product [1..4] == 24
  print $ Main.elem 3 [1..4]
  print $ Main.minimum [1..4 :: Int] == 1
  print $ Main.maximum [1..4 :: Int] == 4
  print $ Main.null [1..4] == False
  print $ Main.null [] == True
  print $ Main.length [1..4] == 4
  print $ Main.toList (Just 1) == [1]
  print $ Main.toList (Nothing :: Maybe Int) == []

data Constant b = Constant b
instance Foldable (Constant) where
  foldMap f (Constant b) = f b

_ = foldMap (*2) (Constant 3) :: Sum Int
_ = foldr ((+) . (*2)) (0) (Constant 3) :: Int

data Two a b = Two a b
instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

data Four a b = Four a b b b
instance Foldable (Four a) where
  foldMap f (Four _ x y z) = mconcat . map f $ [x,y,z]

_ = foldMap (*2) (Four "a" 1 2 3) :: Sum Int
_ = foldr ((+) . (*2)) (0) (Four "a" 1 2 3) :: Int

filterF f = foldMap (\x -> [x | f x])