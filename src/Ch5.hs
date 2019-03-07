module Ch5 where

import           Prelude                           hiding ( head
                                                          , last
                                                          )

data Deque a = Deque [a] [a]
  deriving (Eq)

instance Show a => Show (Deque a) where
  show (Deque h t) = show (h ++ reverse t)

-- We only have ~three~ cases:
-- 1. The queue has no elements at all
-- 1. The queue has one element in the front list and none in the back list
-- 2. ~The queue has one element in the back list and none in the front list~ taken care of by fixEmpty: This should _never_ be the case.
-- 4. The queue has > 2 elements (>=1 in each front/back)

-- Reorders Deque elements to retain the properties needed to make it work.
fixEmpty :: Deque a -> Deque a
fixEmpty (Deque []  [] ) = Deque [] []
fixEmpty (Deque []  [x]) = Deque [x] []
fixEmpty (Deque [x] [] ) = Deque [x] []
fixEmpty (Deque [] t) =
  let (start, end) = splitAt (length t `div` 2) t in Deque (reverse end) start
fixEmpty (Deque t []) =
  let (start, end) = splitAt (length t `div` 2) t in Deque start (reverse end)
fixEmpty d = d

empty :: Deque a
empty = Deque [] []

isEmpty :: Deque a -> Bool
isEmpty (Deque [] _) = True
isEmpty _            = False

-- Prepend to a Deque
snoc :: a -> Deque a -> Deque a
snoc x (Deque h t) = fixEmpty $ Deque h (x : t)

last :: Deque a -> Maybe a
last deque = last' (fixEmpty deque)
 where
  last' (Deque []  _      ) = Nothing
  -- If there is only one element in the list, choose that
  last' (Deque [x] _      ) = Just x
  last' (Deque _   (x : _)) = Just x

init :: Deque a -> Deque a
init deque = fixEmpty (init' deque)
 where
  init' (Deque []  _      ) = empty
  -- If there is only one element in the list, remove it
  init' (Deque [_] _      ) = empty
  init' (Deque h   (x : t)) = Deque h t

-- Append to a Deque
cons :: a -> Deque a -> Deque a
cons x (Deque h t) = fixEmpty $ Deque (x : h) t

head :: Deque a -> Maybe a
head deque = head' (fixEmpty deque)
 where
  head' (Deque (a : _) _) = Just a
  head' (Deque []      _) = Nothing

-- | @'tail'@ removes the head of the list and fixes any emptiness that comes
-- from doing so
tail :: Deque a -> Deque a
tail deque = fixEmpty (tail' deque)
 where
  tail' (Deque []      _) = empty
  -- If there is only one element in the list, remove it
  tail' (Deque [_    ] _) = empty
  tail' (Deque (x : h) t) = Deque h t
