module List.LinkedList(
    LinkedList(EmptyList),
    initList,
    getFirst,
    getLast,
    getTail,
    insertFirst,
    insertFirstMaybe,
    insertLast,
    insertLastMaybe
) where

data LinkedList a = EmptyList | Lin a (LinkedList a) deriving (Show, Read, Eq)

instance Functor LinkedList where
    fmap f EmptyList = EmptyList
    fmap f (Lin x EmptyList) = Lin (f x) EmptyList
    fmap f list@(Lin x rest) = insertFirst (f x) (fmap f (getTail list))

initList :: a -> LinkedList a
initList x = Lin x EmptyList

getFirst :: LinkedList a -> Maybe a
getFirst EmptyList = Nothing
getFirst (Lin x _) = Just x

getLast :: LinkedList a -> Maybe a
getLast EmptyList = Nothing
getLast (Lin x EmptyList) = Just x
getLast (Lin _ rest) = getLast rest

getTail :: LinkedList a -> LinkedList a
getTail EmptyList = error "Empty Linked list has no tail :("
getTail (Lin _ rest) = rest

insertFirst :: a -> LinkedList a -> LinkedList a
insertFirst x list = Lin x list

insertFirstMaybe :: Maybe a -> LinkedList a -> LinkedList a
insertFirstMaybe (Just x) list = insertFirst x list
insertFirstMaybe Nothing list = list

insertLast :: a -> LinkedList a -> LinkedList a
insertLast x EmptyList = error "Cannot append to an empty list."
insertLast x (Lin y EmptyList) = Lin y (Lin x EmptyList)
insertLast x list = insertFirstMaybe (getFirst list) (insertLast x (getTail list))

insertLastMaybe :: Maybe a  -> LinkedList a -> LinkedList a
insertLastMaybe Nothing list = list
insertLastMaybe (Just x) list = insertLast x list