{-# LANGUAGE LambdaCase #-}

module Ch3 where

data Heap a = E | Heap Int (Heap a) a (Heap a)
  deriving (Show)

empty :: Heap a
empty = E

isEmpty :: Heap a -> Bool
isEmpty = \case
  E -> True
  _ -> False

merge :: Ord a => Heap a -> Heap a -> Heap a
merge E    heap = heap
merge heap E    = heap
merge heap1@(Heap _ left1 a right1) heap2@(Heap _ left2 b right2)
  |
  -- Everything in 'left1' is < a (and b)
    a < b     = makeHeapNode left1 a (merge right1 heap2)
  |
  -- Everything in 'left2' is < b (and a)
    otherwise = makeHeapNode left2 b (merge heap1 right2)

-- Make a balanced Heap node with the leftist property
makeHeapNode :: Heap a -> a -> Heap a -> Heap a
makeHeapNode left a right
  | rank left >= rank right = Heap (rank right + 1) left a right
  |
  -- Swap the right and left if the right is larger than the left to preserve
  -- the leftist property
    otherwise               = Heap (rank left + 1) right a left

-- Length of the right spine of a @'Heap'@
rank :: Heap a -> Int
rank E              = 0
rank (Heap r _ _ _) = r

insert :: Ord a => a -> Heap a -> Heap a
insert a = merge (makeHeapNode empty a empty)

findMin :: Heap a -> Maybe a
findMin E                       = Nothing
findMin (Heap _ _ minElement _) = Just minElement

deleteMin :: Ord a => Heap a -> Heap a
deleteMin E                     = E
deleteMin (Heap _ left _ right) = merge left right

fromList :: Ord a => [a] -> Heap a
fromList = fromList' . map singleton
 where
  singleton a = makeHeapNode empty a empty

  fromList' []     = E
  fromList' [x]    = x
  fromList' [x, y] = x `merge` y
  fromList' xs     = fromList' $ mergePairs xs
   where
    mergePairs []           = []
    mergePairs [x         ] = [x]
    mergePairs (x : y : xs) = x `merge` y : mergePairs xs
