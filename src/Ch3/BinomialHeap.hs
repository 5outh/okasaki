{-# LANGUAGE TupleSections #-}
module Ch3.BinomialHeap where

import           Data.List                                ( foldl' )

data Tree a = Node a [Tree a]
  deriving Show

root :: Tree a -> a
root (Node a _) = a

rank :: (Tree a, Int) -> Int
rank (_, r) = r

-- | Link two trees, maintaining heap order.
--
-- - Trees must always be of equal rank.
-- - Larger items are placed at the head of child list
-- - Smaller item forms root, maintaining fast 'getMin'
--
link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node x1 c1) t2@(Node x2 c2) | x1 <= x2  = Node x1 (t2 : c1)
                                     | otherwise = Node x2 (t1 : c2)

-- A collection of heap-ordered trees in which no two trees have the same rank
--
-- Stored in increasing order of rank
type BinomialHeap a = [(Tree a, Int)]

insTree :: Ord a => (Tree a, Int) -> BinomialHeap a -> BinomialHeap a
insTree tree [] = [tree]
insTree tree@(tree', r) trees@(htree@(htree', _) : htrees)
  | rank tree < rank htree  = tree : trees
  | rank tree == rank htree = insTree (link tree' htree', r + 1) htrees
  | otherwise               = error "attempt to link trees of nonidentical rank"

insert :: Ord a => a -> BinomialHeap a -> BinomialHeap a
insert x = insTree (Node x [], 0)

-- This is like insertion sort (using rank), but merging any identically ranked trees.
merge :: Ord a => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
merge ts1 []  = ts1
merge []  ts2 = ts2
merge ts1@(t1@(t1', r) : ts'1) ts2@(t2@(t2', _) : ts'2)
  | rank t1 < rank t2 = t1 : merge ts'1 ts2
  | rank t1 > rank t2 = t2 : merge ts1 ts'2
  | otherwise         = insTree (link t1' t2', r + 1) (merge ts'1 ts'2)

-- | Find the Tree with the minimal root in a BinomialHeap and return it.
removeMinTree :: Ord a => BinomialHeap a -> ((Tree a, Int), BinomialHeap a)
removeMinTree [t] = (t, [])
removeMinTree (t : ts) =
  let (t', ts') = removeMinTree ts
  in  if root (fst t) < root (fst t') then (t, ts) else (t', t : ts')

-- | Pull the minimum value from a BinomialHeap
findMin :: Ord a => BinomialHeap a -> a
findMin ts = let ((t, _), _) = removeMinTree ts in root t

-- | Delete the minimum value from a BinomialHeap
--
-- Note: Children are sorted in decreasing order of rank, so we reverse them
-- to satisfy the BinomialHeap ordering before merging them with the rest of
-- the heap.
--
-- TODO: Is this rank correct?
deleteMin :: Ord a => BinomialHeap a -> BinomialHeap a
deleteMin ts =
  let ((Node x ts1, r), ts2) = removeMinTree ts
  in  merge (map (, r - 1) $ reverse ts1) ts2

fromList :: Ord a => [a] -> BinomialHeap a
fromList = foldl' (flip insert) []
