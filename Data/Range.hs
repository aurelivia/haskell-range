{-# LANGUAGE MultiWayIf #-}

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

import Data.List (intercalate, foldl', sort)

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
  x' = foldl' (\x' -> \y -> foldl' (\x'' -> \x -> (diff x y) ++ x'') [] x') [x] ys
  diff (Span xlb xub _) (Span ylb yub _) = if (ylb >= xlb && ylb <= xub)
    then if (yub >= xlb && yub <= xub)
      then [spanOf xlb ylb, spanOf yub xub]
      else [spanOf xlb ylb]
    else if (yub >= xlb && yub <= xub)
      then [spanOf ylb xub]
      else [x]

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

instance (Eq a, Show a) => Show (Range a) where
  showsPrec i (Range sps _) = (++) $ '[' : (intercalate ", " sps') ++ "]" where
    sps' = map (\(Span lb ub _) -> if lb == ub then (showsPrec i lb $ "") else (showsPrec i lb $ "..") ++ (showsPrec i ub $ "")) sps

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
fromList :: (Ord a, Enum a) => [a] -> Range a
fromList = unions . map singleton

-- | Creates a new range covering an existing range plus an additional element
include :: (Ord a, Enum a) => a -> Range a -> Range a
include x r
  | within x r = r
  | otherwise  = union r $ singleton x

-- | Creates a new range covering an existing range plus a set of elements
includes :: (Ord a, Enum a, Foldable t) => t a -> Range a -> Range a
includes xs r = foldl' (\r' -> \x -> include x r') r xs

remove :: (Ord a, Enum a) => a -> Range a -> Range a
remove x r
  | within x r = difference r $ singleton x
  | otherwise  = r

removes :: (Ord a, Enum a, Foldable t) => t a -> Range a -> Range a
removes xs r = foldl' (\r' -> \x -> remove x r') r xs



-- | Checks whether a given element falls within a given range
within :: Ord a => a -> Range a -> Bool
within x (Range sps _) = any (\s -> inSpan x s) sps
{-# INLINE within #-}

-- | Checks whether a given range contains a given element
contains :: Ord a => Range a -> a -> Bool
contains (Range sps _) x = any (\s -> inSpan x s) sps
{-# INLINE contains #-}




fixLength :: [Span a] -> Range a
fixLength sp = Range sp $ foldl' (\s -> \(Span _ _ l) -> s + l) 0 sp

-- | Creates a new range consiting of a union of two ranges.
union :: (Ord a, Enum a) => Range a -> Range a -> Range a
union (Range xsp _) (Range ysp _) = fixLength $ unionSpan (xsp ++ ysp)

-- | Creates a new range that is a union of a list of ranges.
unions :: (Ord a, Enum a) => [Range a] -> Range a
unions = foldl' (\r -> \x -> union x r) empty

-- | Calculates the difference between two ranges, subtracting the right argument from the left
difference :: (Ord a, Enum a) => Range a -> Range a -> Range a
difference (Range xsp _) (Range ysp _) = fixLength $ differenceSpan xsp ysp





-- | Gets the nth element from the lower bound of a range, of the values within a range
getNth :: (Ord a, Enum a) => Int -> Range a -> a
getNth i (Range sps len)
  | i >= len  = error "Out of range."
  | otherwise = getNth' i sps
  where
    getNth' i (x@(Span _ _ l):xs)
      | i >= l    = getNth' (i - l) xs
      | otherwise = getNthOfSpan i x