{-# LANGUAGE LambdaCase #-}

module Data.IntervalMap
  ( -- * Intervals
    IntervalBound(..)
  , Interval
  , interval

    -- * Interval maps
  , IntervalMap
  , empty
  , size
  , lookup
  , insert
  , delete
  , toList
  , fromList
  ) where

import Prelude hiding (lookup)

import Control.Monad (foldM, guard)

-- | Left or right bound of an interval.
data IntervalBound b
  = Closed b -- ^ Including this value.
  | Open b   -- ^ Excluding this value.
  | Infinity -- ^ +inf on the right, -inf on the left.
  deriving (Eq, Show)

-- | Interval of values used as keys in the interval maps.
data Interval b
  = Interval (IntervalBound b) (IntervalBound b)
  deriving (Eq, Show)

-- | Smart constructor for intervals which checks the bounds are not backward
-- and the interval is not empty.
--
-- The lowest bound must be less than upper bound except for
-- @interval (Closed 1) (Closed 1)@, a singleton and
-- @interval Infinity Infinity@, (-inf, +inf).
interval :: Ord b => IntervalBound b -> IntervalBound b -> Maybe (Interval b)
interval lb ub = case (lb, ub) of
  (Closed x, Closed y)
    | x <= y -> pure $ Interval lb ub
  (Open x, Closed y)
    | x < y -> pure $ Interval lb ub
  (Open x, Open y)
    | x < y -> pure $ Interval lb ub
  (Closed x, Open y)
    | x < y -> pure $ Interval lb ub
  (Infinity, _) -> pure $ Interval lb ub
  (_, Infinity) -> pure $ Interval lb ub
  _ -> Nothing

data IntervalResult
  = Below
  | Overlap
  | Above

inInterval :: Ord b => b -> Interval b -> IntervalResult
inInterval x = \case
  Interval (Closed lb) _
    | x < lb -> Below
  Interval (Open lb) _
    | x <= lb -> Below
  Interval _ (Closed ub)
    | ub < x -> Above
  Interval _ (Open ub)
    | ub <= x -> Above
  _ -> Overlap

compareIntervals :: Ord b => Interval b -> Interval b -> IntervalResult
compareIntervals = curry $ \case
  (Interval _ (Closed ub), Interval (Closed lb) _)
    | ub < lb -> Below
  (Interval _ (Open ub), Interval (Closed lb) _)
    | ub <= lb -> Below
  (Interval _ (Open ub), Interval (Open lb) _)
    | ub <= lb -> Below
  (Interval _ (Closed ub), Interval (Open lb) _)
    | ub <= lb -> Below

  (Interval (Closed lb) _, Interval _ (Closed ub))
    | ub < lb -> Above
  (Interval (Closed lb) _, Interval _ (Open ub))
    | ub <= lb -> Above
  (Interval (Open lb) _, Interval _ (Open ub))
    | ub <= lb -> Above
  (Interval (Open lb) _, Interval _ (Closed ub))
    | ub <= lb -> Above

  _ -> Overlap

-- | Structure mapping intervals to values. Is is very similar to a classic map
-- (as in 'Data.Map.Strict') but keys are intervals and lookups are made with
-- values with one of the intervals.
data IntervalMap b a
  = Node Int (Interval b) a (IntervalMap b a) (IntervalMap b a)
  | Leaf

-- | Empty interval map.
empty :: IntervalMap b a
empty = Leaf

-- | Return the number of elements in the interval map. /O(1)/
size :: IntervalMap b a -> Int
size = \case
  Leaf -> 0
  Node n _ _ _ _ -> n

-- | Find the value corresponding to the interval containing the given
-- parameter. /O(log n)/
lookup :: Ord b => b -> IntervalMap b a -> Maybe a
lookup x = \case
  Leaf -> Nothing
  Node _ interval value leftMap rightMap ->
    case x `inInterval` interval of
      Below   -> lookup x leftMap
      Above   -> lookup x rightMap
      Overlap -> Just value

-- | Delete the left-most element in the tree, i.e. the lowest interval.
deleteLeftMost
  :: Ord b
  => IntervalMap b a -> Maybe (IntervalMap b a, Interval b, a)
deleteLeftMost = \case
  Leaf -> Nothing
  Node _ interval value Leaf Leaf ->
    pure (Leaf, interval, value)
  Node _ interval value Leaf rightMap ->
    pure (rightMap, interval, value)
  Node n interval value leftMap rightMap -> do
    (leftMap', interval', value') <- deleteLeftMost leftMap
    node' <- balanceLeft $ Node (n-1) interval value leftMap' rightMap
    pure (node', interval', value')

-- | Delete the right-most element in the tree, i.e. the highest interval.
deleteRightMost
  :: Ord b
  => IntervalMap b a
  -> Maybe (IntervalMap b a, Interval b, a)
deleteRightMost = \case
  Leaf -> Nothing
  Node _ interval value Leaf Leaf ->
    pure (Leaf, interval, value)
  Node _ interval value leftMap Leaf ->
    Just (leftMap, interval, value)
  Node n interval value leftMap rightMap -> do
    (rightMap', interval', value') <- deleteRightMost rightMap
    node' <- balanceRight $ Node (n-1) interval value leftMap rightMap'
    pure (node', interval', value')

-- | Balance map if the left side is too small.
balanceLeft :: Ord b => IntervalMap b a -> Maybe (IntervalMap b a)
balanceLeft = \case
  Leaf -> pure Leaf
  m@(Node _ _ _ _ Leaf) -> pure m
  Node n interval value leftMap rightMap
    | size leftMap < size rightMap + 1 -> do
        leftMap' <- insert interval value leftMap
        (rightMap', interval', value') <- deleteLeftMost rightMap
        Node n interval' value'
          <$> pure leftMap'
          <*> balanceLeft rightMap'
    | otherwise ->
        Node n interval value
          <$> balanceLeft leftMap
          <*> pure rightMap

-- | Balance map if the right side is too small.
balanceRight :: Ord b => IntervalMap b a -> Maybe (IntervalMap b a)
balanceRight = \case
  Leaf -> pure Leaf
  m@(Node _ _ _ Leaf _) -> pure m
  Node n interval value leftMap rightMap
    | size rightMap < size leftMap + 1 -> do
        rightMap' <- insert interval value rightMap
        (leftMap', interval', value') <- deleteRightMost leftMap
        Node n interval' value'
          <$> balanceRight leftMap'
          <*> pure rightMap'
    | otherwise ->
        Node n interval value
          <$> pure leftMap
          <*> balanceRight rightMap

-- | Remove the interval (and its value) containing the given value from the
-- interval map.
delete
  :: Ord b
  => b -> IntervalMap b a -> Maybe (IntervalMap b a)
delete x = \case
  Leaf -> pure Leaf
  Node n interval value leftMap rightMap ->
    case x `inInterval` interval of
      Below -> do
        leftMap' <- delete x leftMap
        balanceLeft $ Node (n-1) interval value leftMap' rightMap
      Above -> do
        rightMap' <- delete x rightMap
        balanceRight $ Node (n-1) interval value leftMap rightMap'
      Overlap
        | size leftMap == 0 -> pure rightMap
        | size rightMap == 0 -> pure leftMap
        | size leftMap < size rightMap -> do
            (rightMap', interval', value') <- deleteLeftMost rightMap
            balanceRight $ Node (n-1) interval' value' leftMap rightMap'
        | otherwise -> do
            (leftMap', interval', value') <- deleteRightMost leftMap
            balanceLeft $ Node (n-1) interval' value' leftMap' rightMap

-- | Insert a new value for an interval map. Fail on interval overlap.
insert
  :: Ord b
  => Interval b -> a -> IntervalMap b a -> Maybe (IntervalMap b a)
insert interval value = \case
  Leaf -> pure $ Node 1 interval value Leaf Leaf

  Node n rootInterval rootValue leftMap rightMap ->
    case compareIntervals interval rootInterval of
      Overlap -> Nothing
      Below -> do
        leftMap' <- insert interval value leftMap
        balanceRight $ Node (n+1) rootInterval rootValue leftMap' rightMap
      Above -> do
        rightMap' <- insert interval value rightMap
        balanceLeft $ Node (n+1) rootInterval rootValue leftMap rightMap'

-- | Transform an interval map into a list of pairs ordered from lowest to
-- highest interval.
toList :: IntervalMap b a -> [(Interval b, a)]
toList =
    go []
  where
    go :: [(Interval b, a)] -> IntervalMap b a -> [(Interval b, a)]
    go acc = \case
      Leaf -> acc
      Node _ interval value leftMap rightMap ->
        let acc'  = go acc rightMap
            acc'' = (interval, value) : acc'
        in go acc'' leftMap

-- | Construct an interval map from a list of pairs. Fail on interval overlap.
-- /O(n)/
fromList :: Ord b => [(Interval b, a)] -> Maybe (IntervalMap b a)
fromList =
  foldM (\acc (interval, value) -> insert interval value acc) empty
