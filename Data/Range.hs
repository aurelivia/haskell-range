module Data.Range
  -- * Datatypes
  ( Range(rangeLength)
  -- * Range Constructors
  , empty, singleton, range
  -- * Queries
  , contains
  ) where

import Data.List (intercalate)

data Span a = Span
  { lowerBound :: a
  , upperBound :: a
  , spanLength :: Int
  } deriving (Eq)

spanOf :: (Ord a, Enum a) => a -> a -> Span a
spanOf lb ub
  | lb > ub   = spanOf ub lb
  | otherwise = Span lb ub ((fromEnum ub) - (fromEnum lb))

inSpan :: (Ord a) => a -> Span a -> Bool
inSpan x (Span lb ub _) = x >= lb && x <= ub
{-# INLINE inSpan #-}





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
contains :: (Ord a) => Range a -> a -> Bool
contains (Range sps _) x = any (\s -> inSpan x s) sps
{-# INLINE contains #-}