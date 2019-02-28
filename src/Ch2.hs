{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Ch2 where

import           Prelude                           hiding ( lookup )

-- Show that this can be generated in O(n) time and O(n) space
suffixes :: [a] -> [[a]]
suffixes []          = [] -- O(1) Operation
suffixes ys@(_ : xs) = ys : suffixes xs
--                     ^^   ^^^^^^^^^^^
--                    O(1)    O(N)
--
--
-- We need to reference each individual element. I will call these suffix1..suffixN
--
-- For example, the suffixes of the list [a,b,c,d] look like this:
--
-- xs---->[a] - [b] - [c] - [d] - E
-- suffix1-^     ^     ^     ^    ^
--       suffix2-^     ^     ^    ^
--             suffix3-^     ^    ^
--                   suffix4-^    ^
--                        suffix5-^
--
-- All we have to do is create N+1 new pointers (O(N)),
-- where N is the length of the list, since each element's tail is exactly the
-- next element in the list, and can be shared.



class Set set a | set a -> a where
  empty :: set a
  member :: a -> set a -> Bool
  insert :: a -> set a -> set a

class FiniteMap finiteMap k a where
  fmempty :: finiteMap k a
  bind :: k -> a -> finiteMap k a -> finiteMap k a
  lookup :: k -> finiteMap k a -> Maybe a

data BinaryTree a = Empty | BinaryTree (BinaryTree a) a (BinaryTree a)
  deriving Show

data TaggedBinaryTree k a
  = TEmpty
  | TaggedBinaryTree k (TaggedBinaryTree k a) a (TaggedBinaryTree k a)
  deriving Show

instance Ord a => Set BinaryTree a where
  empty = Empty

  member a Empty = False
  member a (BinaryTree left x right)
    | a < x = member a left
    | a > x = member a right
    | otherwise = True

  insert a Empty = BinaryTree empty a empty
  insert a tree@(BinaryTree left x right)
    | a < x = insert a left
    | a > x = insert a right
    | otherwise = tree -- Element was already inserted

-- | Create a complete tree of depth @d0@ filled with @x0@
--
complete :: (Num t, Eq t, Ord a) => a -> t -> BinaryTree a
complete x0 d0 = go x0 d0 empty
 where
  go x 0 tree = tree
  go x d tree = go x (d - 1) $ BinaryTree tree x tree

-- TODO: Revisit this
--
-- | Create a balanced tree of size @s@ filled with @x@s
balanced x0 s0 = let (l, r) = create2 x0 (pred s0) in BinaryTree l x0 r
 where
  create2 x s =
    let halfSize = s `div` 2
        addSize  = s `mod` 2
    in  (complete x halfSize, complete x (halfSize + addSize))

instance Ord k => FiniteMap TaggedBinaryTree k a where
  fmempty = TEmpty
  bind k a = \case
    TEmpty -> TaggedBinaryTree k TEmpty a TEmpty
    tree@(TaggedBinaryTree k1 left a1 right) ->
      case () of
      () | k < k1 -> bind k a left
         | k > k1 -> bind k a right
         | otherwise -> tree
  lookup k = \case
    TEmpty -> Nothing
    tree@(TaggedBinaryTree k1 left a1 right) ->
      case () of
      () | k < k1 -> lookup k left
         | k > k1 -> lookup k right
         | otherwise -> Just a1
