{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
module Data.Range.Internal where

import Data.List (foldl', sort, intercalate)

data Span a = Span
  { lowerBound :: a
  , upperBound :: a
  , spanLength :: Int
  } deriving (Eq, Show)

instance Ord a => Ord (Span a) where
  compare (Span xlb xub _) (Span ylb yub _) = if
    | xlb < ylb -> LT
    | ylb < xlb -> GT
    | xub < yub -> LT
    | yub < xub -> GT
    | otherwise -> EQ

-- | A range.
data Range a = Range
  { spans :: [ Span a ]
  , rangeLength :: Int -- ^ Retrieves the total number of elements within the range.
  } deriving (Eq)

fixLength :: [Span a] -> Range a
fixLength sp = Range sp $ foldl' (\s -> \(Span _ _ l) -> s + l) 0 sp

instance (Eq a, Show a) => Show (Range a) where
  showsPrec i (Range sps _) = (++) $ '[' : (intercalate ", " sps') ++ "]" where
    sps' = map (\(Span lb ub _) -> if lb == ub
        then (showsPrec i lb $ "")
        else (showsPrec i lb $ "..") ++ (showsPrec i ub $ "")
      ) sps

spanOf :: (Ord a, Enum a) => a -> a -> Span a
spanOf lb ub
  | lb > ub   = spanOf ub lb
  | otherwise = Span lb ub (1 + (fromEnum ub) - (fromEnum lb))

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
      | xub >= ylb || (succ xub) == ylb = (Span xlb yub (xi + yi)) : ys
      | otherwise                       = x : ya

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
    | spanContains y x  -> unionSpan' (x : ys)
    | spanOverlaps x y  -> unionSpan' $ (mergeSpans x y) : ys
    | (succ xub) == ylb -> unionSpan' $ (mergeSpans x y) : ys
    | otherwise         -> x : unionSpan' xs

intersectionSpan :: (Ord a, Enum a) => [Span a] -> [Span a]
intersectionSpan = insctSpan' . sort where
  insctSpan' [] = []
  insctSpan' (x:[]) = [x]
  insctSpan' (x@(Span xlb xub _):xs@(y@(Span ylb yub _):ys)) = if
    | spanContains x y -> insctSpan' (x:ys)
    | spanOverlaps x y -> insctSpan' $ (spanOf (max xlb ylb) (min xub yub)) : ys
    | otherwise        -> insctSpan' ys

differenceSpan :: (Ord a, Enum a) => [Span a] -> [Span a] -> [Span a]
differenceSpan [] _ = []
differenceSpan (x:xs) ys = x' ++ differenceSpan xs ys where
  x' = foldl' (\x' -> \y -> foldl' (\x'' -> \x -> (diff x y) ++ x'') [] x') [x] ys
  diff x@(Span xlb xub _) (Span ylb yub _) = if
    | ylb == xlb -> if (yub < xub) then [spanOf (succ yub) xub] else []
    | ylb >  xlb -> if (yub < xub)
                      then [spanOf xlb (pred ylb), spanOf (succ yub) xub]
                      else if (ylb > xub) then [x] else [spanOf xlb (pred ylb)]
    | yub <  xub -> if (yub < xlb) then [x] else [spanOf (succ yub) xub]
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