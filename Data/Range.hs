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

import Data.Range.Internal

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
range lb ub = let l = 1 + (fromEnum ub) - (fromEnum lb) in Range [Span lb ub l] l
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