{-# LANGUAGE MultiWayIf #-}

module Data.Range
  -- * Datatypes
  ( Range(rangeLength)
  -- * Range Constructors
  , empty, singleton, range
  -- * Queries
  , contains
  -- * Operations
  , union
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
  | otherwise = Span lb ub ((fromEnum ub) - (fromEnum lb))

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
  unionSpan' (x:xs@(y:ys)) = if
    | spanContains x y -> unionSpan' xs
    | spanOverlaps x y -> unionSpan' $ (mergeSpans x y) : ys
    | otherwise        -> x : unionSpan' xs







-- | A range.
data Range a = Range
  { spans :: [ Span a ]
  , rangeLength :: Int -- ^ Retrieves the total number of elements within the range.
  } deriving (Eq)

instance Show a => Show (Range a) where
  showsPrec i (Range sps _) = (++) $ '[' : (intercalate ", " sps') ++ "]" where
    sps' = map (\(Span lb ub _) -> (showsPrec i lb $ "..") ++ (showsPrec i ub $ "")) sps

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



-- | Checks whether a given element falls within a given range
contains :: Ord a => Range a -> a -> Bool
contains (Range sps _) x = any (\s -> inSpan x s) sps
{-# INLINE contains #-}





-- | Creates a new range consiting of a union of two ranges.
union :: (Ord a, Enum a) => Range a -> Range a -> Range a
union (Range xsp _) (Range ysp _) = Range sp' (foldl' (\s -> \(Span _ _ l) -> s + l) 0 sp') where
  sp' = unionSpan (xsp ++ ysp)