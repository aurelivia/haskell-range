{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}

module Data.Range
  ( -- * Datatypes
    Range(rangeLength)
  -- * Range Constructors
  , empty, singleton, range, fromList
  , include, includes, remove, removes
  -- * Queries
  , within, contains
  -- * Operations
  , union, unions, difference
  -- * Iteration
  , getNth
  ) where

import Prelude hiding (foldr, foldl)
import Data.List (intercalate, sort)
import qualified Data.List as List

data Span a = Span
  { lowerBound :: a
  , upperBound :: a
  , spanLength :: Int
  } deriving (Eq)

instance Ord a => Ord (Span a) where
  compare (Span xlb xub _) (Span ylb yub _) = if
    | xlb < ylb -> LT
    | ylb < xlb -> GT
    | xub < yub -> LT
    | yub < xub -> GT
    | otherwise -> EQ

spanOf :: (Ord a, Enum a) => a -> a -> Span a
spanOf lb ub
  | lb > ub   = spanOf ub lb
  | otherwise = Span lb ub ((+) 1 $ (fromEnum ub) - (fromEnum lb))

inSpan :: Ord a => a -> Span a -> Bool
inSpan x (Span lb ub _) = x >= lb && x <= ub
{-# INLINE inSpan #-}

insert :: (Ord a, Enum a) => a -> [Span a] -> [Span a]
insert x [] = [(Span x x 1)]
insert x ya@(y@(Span lb ub i):ys)
  | x >= lb && x <= ub = ya
  | x == (pred lb)     = (Span x ub (i + 1)) : ys
  | x < lb             = (Span x x 1) : ya
  | x == (succ ub)     = join (Span lb x (i + 1)) ys
  | otherwise          = join y $ insert x ys
  where
    join x [] = [x]
    join x@(Span xlb xub xi) ya@((Span ylb yub yi):ys)
      | xub >= ylb = (Span xlb yub ((xi + yi) - 1)) : ys
      | otherwise  = x : ya

spanContains :: Ord a => Span a -> Span a -> Bool
spanContains (Span xlb xub _) (Span ylb yub _) = xlb >= ylb && xlb <= yub
  && xub >= ylb && xub <= yub

spanOverlaps :: Ord a => Span a -> Span a -> Bool
spanOverlaps (Span xlb xub _) (Span ylb yub _) = (xlb >= ylb && xlb <= yub)
  || (xub >= ylb && xub <= yub)

mergeSpans :: (Ord a, Enum a) => Span a -> Span a -> Span a
mergeSpans (Span xlb xub _) (Span ylb yub _) = spanOf (min xlb ylb) (max xub yub)

unionSpan :: (Ord a, Enum a) => [Span a] -> [Span a]
unionSpan = unionSpan' . sort where
  unionSpan' [] = []
  unionSpan' (x:[]) = [x]
  unionSpan' (x@(Span _ xub _):xs@(y@(Span ylb _ _):ys)) = if
    | spanContains x y  -> unionSpan' xs
    | spanOverlaps x y  -> unionSpan' $ (mergeSpans x y) : ys
    | (succ xub) == ylb -> unionSpan' $ (mergeSpans x y) : ys
    | otherwise         -> x : unionSpan' xs

differenceSpan :: (Ord a, Enum a) => [Span a] -> [Span a] -> [Span a]
differenceSpan [] _ = []
differenceSpan (x:xs) ys = x' ++ differenceSpan xs ys where
  x' = List.foldl' (\x' -> \y -> List.foldl' (\x'' -> \x -> (diff x y) ++ x'') [] x') [x] ys
  diff x@(Span xlb xub _) (Span ylb yub _) = if
    | ylb == xlb -> if (yub < xub) then [spanOf (succ yub) xub] else []
    | ylb >  xlb -> if (yub < xub)
                      then [spanOf xlb (pred ylb), spanOf (succ yub) xub]
                      else if (ylb > xub) then [x] else [spanOf xlb (pred ylb)]
    | yub <  xub -> if (yub < ylb) then [x] else [spanOf (succ yub) xub]
    | otherwise  -> []


foldSpanR :: (Ord a, Enum a) => (a -> b -> b) -> b -> a -> a -> b
foldSpanR fn z lb ub
  | lb == ub  = fn lb z
  | otherwise = foldSpanR fn (fn ub z) lb (pred ub)

foldSpanR' :: (Ord a, Enum a) => (a -> b -> b) -> b -> a -> a -> b
foldSpanR' fn z lb ub
  | lb == ub  = let !z' = fn lb z in z'
  | otherwise = let !z' = fn ub z in foldSpanR' fn z' lb (pred ub)

foldSpanL :: (Ord a, Enum a) => (b -> a -> b) -> b -> a -> a -> b
foldSpanL fn z lb ub
  | lb == ub  = fn z ub
  | otherwise = foldSpanL fn (fn z lb) (succ lb) ub

foldSpanL' :: (Ord a, Enum a) => (b -> a -> b) -> b -> a -> a -> b
foldSpanL' fn z lb ub
  | lb == ub  = let !z' = fn z ub in z'
  | otherwise = let !z' = fn z lb in foldSpanL fn z' (succ lb) ub

getNthOfSpan :: (Ord a, Enum a) => Int -> Span a -> a
getNthOfSpan 0 (Span lb _ _) = lb
getNthOfSpan i (Span lb ub len)
  | i == (len - 1)  = ub
  | i > len         = error "Out of bounds."
  | i <= mid        = getNth' succ i lb
  | otherwise       = getNth' pred (i - mid) ub
  where
    mid = div ((fromEnum ub) - (fromEnum lb)) 2
    getNth' _  0 x = x
    getNth' fn i x = getNth' fn (i - 1) (fn x)







-- | A range.
data Range a = Range
  { spans :: [ Span a ]
  , rangeLength :: Int -- ^ Retrieves the total number of elements within the range.
  } deriving (Eq)

fixLength :: [Span a] -> Range a
fixLength sp = Range sp $ List.foldl' (\s -> \(Span _ _ l) -> s + l) 0 sp

instance (Eq a, Show a) => Show (Range a) where
  showsPrec i (Range sps _) = (++) $ '[' : (intercalate ", " sps') ++ "]" where
    sps' = map (\(Span lb ub _) -> if lb == ub
        then (showsPrec i lb $ "")
        else (showsPrec i lb $ "..") ++ (showsPrec i ub $ "")
      ) sps

instance (Ord a, Enum a) => Semigroup (Range a) where
  (<>) = union

instance (Ord a, Enum a) => Monoid (Range a) where
  mempty = empty


-- | Constructs an empty range.
empty :: Range a
empty = Range [] 0

-- | Constructs a range of a single element.
singleton :: a -> Range a
singleton x = Range [Span x x 1] 1

-- | Constructs a new range from a given lower bound to a given upper bound (both inclusive.)
range :: (Ord a, Enum a) => a -> a -> Range a
range lb ub = let l = (fromEnum ub) - (fromEnum lb) in Range [Span lb ub l] l
{-# INLINE range #-}

-- | Constructs a range from a list of values, where the resulting range covers only the values in the list.
fromList :: (Ord a, Enum a, Foldable t) => t a -> Range a
fromList = fixLength . List.foldl' (\sps -> \x -> insert x sps) []

-- | Creates a new range covering an existing range plus an additional element
include :: (Ord a, Enum a) => a -> Range a -> Range a
include x (Range sps _) = fixLength $ insert x sps

-- | Creates a new range covering an existing range plus a set of elements
includes :: (Ord a, Enum a, Foldable t) => t a -> Range a -> Range a
includes xs r = union r $ fromList xs

remove :: (Ord a, Enum a) => a -> Range a -> Range a
remove x r
  | within x r = difference r $ singleton x
  | otherwise  = r

removes :: (Ord a, Enum a, Foldable t) => t a -> Range a -> Range a
removes xs r = difference r $ fromList xs



-- | Checks whether a given element falls within a given range
within :: Ord a => a -> Range a -> Bool
within x (Range sps _) = any (\s -> inSpan x s) sps
{-# INLINE within #-}

-- | Checks whether a given range contains a given element
contains :: Ord a => Range a -> a -> Bool
contains (Range sps _) x = any (\s -> inSpan x s) sps
{-# INLINE contains #-}




-- | Creates a new range consiting of a union of two ranges.
union :: (Ord a, Enum a) => Range a -> Range a -> Range a
union (Range xsp _) (Range ysp _) = fixLength $ unionSpan (xsp ++ ysp)

-- | Creates a new range that is a union of a list of ranges.
unions :: (Ord a, Enum a, Foldable t) => t (Range a) -> Range a
unions = List.foldl' (\r -> \x -> union x r) empty

-- | Calculates the difference between two ranges, subtracting the right argument from the left
difference :: (Ord a, Enum a) => Range a -> Range a -> Range a
difference (Range xsp _) (Range ysp _) = fixLength $ differenceSpan xsp ysp





foldr :: (Ord a, Enum a) => (a -> b -> b) -> b -> Range a -> b
foldr _ z (Range [] 0) = z
foldr fn z (Range sps _) = List.foldr (\(Span lb ub _) -> \z' -> foldSpanR fn z' lb ub) z sps

foldr' :: (Ord a, Enum a) => (a -> b -> b) -> b -> Range a -> b
foldr' _ z (Range [] 0) = z
foldr' fn z (Range sps _) = foldr'' (\(Span lb ub _) -> \z' -> foldSpanR' fn z' lb ub) z sps where
  foldr'' fn z []     = z
  foldr'' fn z (x:xs) = let !z' = fn x (foldr'' fn z xs) in z'

foldl :: (Ord a, Enum a) => (b -> a -> b) -> b -> Range a -> b
foldl _ z (Range [] 0) = z
foldl fn z (Range sps _) = List.foldl (\z' -> \(Span lb ub _) -> foldSpanL fn z' lb ub) z sps

foldl' :: (Ord a, Enum a) => (b -> a -> b) -> b -> Range a -> b
foldl' _ z (Range [] 0) = z
foldl' fn z (Range sps _) = List.foldl' (\z' -> \(Span lb ub _) -> foldSpanL' fn z' lb ub) z sps

-- | Gets the nth element from the lower bound of a range, of the values within a range
getNth :: (Ord a, Enum a) => Int -> Range a -> a
getNth i (Range sps len)
  | i >= len  = error "Out of range."
  | otherwise = getNth' i sps
  where
    getNth' i (x@(Span _ _ l):xs)
      | i >= l    = getNth' (i - l) xs
      | otherwise = getNthOfSpan i x