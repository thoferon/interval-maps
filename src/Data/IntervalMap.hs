{-# LANGUAGE LambdaCase #-}

module Data.IntervalMap
  (
  ) where

import Prelude hiding (lookup)

data Interval b
  = ClosedInterval b b
  deriving (Eq, Show)

data IntervalResult
  = Below
  | Overlap
  | Above

inInterval :: Ord b => b -> Interval b -> IntervalResult
inInterval x (ClosedInterval lb ub)
  | x < lb    = Below
  | x > ub    = Above
  | otherwise = Overlap

compareIntervals :: Ord b => Interval b -> Interval b -> IntervalResult
compareIntervals (ClosedInterval lb ub) (ClosedInterval lb' ub')
  | ub  < lb' = Below
  | ub' < lb  = Above
  | otherwise = Overlap

data IntervalMap b a
  = Node Int (Interval b) a (IntervalMap b a) (IntervalMap b a)
  | Leaf
  deriving (Eq, Show)

size :: IntervalMap b a -> Int
size = \case
  Leaf -> 0
  Node n _ _ _ _ -> n

lookup :: Ord b => b -> IntervalMap b a -> Maybe a
lookup x = \case
  Leaf -> Nothing
  Node _ interval value leftMap rightMap ->
    case x `inInterval` interval of
      Below   -> lookup x leftMap
      Above   -> lookup x rightMap
      Overlap -> Just value

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
