module Example.Example1 where

-- At the root folder, run "cabal run record-builder -v0 Example/Example1.hs".

data Tree a = Empty
            | Leaf { value :: a }
            | Node { left  :: Tree a
                   , value :: a
                   , right :: Tree a }
  deriving (Eq, Ord, Show)

data List a = Nil
            | Cons { element :: a
                   , next    :: List a }
  deriving (Eq, Ord, Show)

newtype VBool = VBool { bool :: Bool }
  deriving (Eq, Ord, Show)

-- The following is generated code.

-- Boilerplates for Tree a.

isEmpty :: Tree a -> Bool
isEmpty Empty {} = True
isEmpty _        = False

isLeaf :: Tree a -> Bool
isLeaf Leaf {} = True
isLeaf _       = False

isNode :: Tree a -> Bool
isNode Node {} = True
isNode _       = False

getLeft :: Tree a -> Maybe (Tree a)
getLeft _mmzkTree = case _mmzkTree of
  Node {} -> Just $ left _mmzkTree
  _       -> Nothing

getRight :: Tree a -> Maybe (Tree a)
getRight _mmzkTree = case _mmzkTree of
  Node {} -> Just $ right _mmzkTree
  _       -> Nothing

getValue :: Tree a -> Maybe (a)
getValue _mmzkTree = case _mmzkTree of
  Leaf {} -> Just $ value _mmzkTree
  Node {} -> Just $ value _mmzkTree
  _       -> Nothing

setLeft :: Tree a -> Tree a -> Tree a
setLeft _mmzkTree _left = case _mmzkTree of
  Node {} -> _mmzkTree { left = _left }
  _       -> _mmzkTree

setRight :: Tree a -> Tree a -> Tree a
setRight _mmzkTree _right = case _mmzkTree of
  Node {} -> _mmzkTree { right = _right }
  _       -> _mmzkTree

setValue :: Tree a -> a -> Tree a
setValue _mmzkTree _value = case _mmzkTree of
  Leaf {} -> _mmzkTree { value = _value }
  Node {} -> _mmzkTree { value = _value }
  _       -> _mmzkTree

modifyLeft :: Tree a -> (Tree a -> Tree a) -> Tree a
modifyLeft _mmzkTree _modifier = case _mmzkTree of
  Node {} -> _mmzkTree { left = _modifier $ left _mmzkTree }
  _       -> _mmzkTree

modifyRight :: Tree a -> (Tree a -> Tree a) -> Tree a
modifyRight _mmzkTree _modifier = case _mmzkTree of
  Node {} -> _mmzkTree { right = _modifier $ right _mmzkTree }
  _       -> _mmzkTree

modifyValue :: Tree a -> (a -> a) -> Tree a
modifyValue _mmzkTree _modifier = case _mmzkTree of
  Leaf {} -> _mmzkTree { value = _modifier $ value _mmzkTree }
  Node {} -> _mmzkTree { value = _modifier $ value _mmzkTree }
  _       -> _mmzkTree

modifyLeft' :: Tree a -> (Tree a -> Tree a) -> Tree a
modifyLeft' _mmzkTree _modifier = case _mmzkTree of
  Node {} -> _mmzkTree { left = left _mmzkTree `seq` _modifier (left _mmzkTree) }
  _       -> _mmzkTree

modifyRight' :: Tree a -> (Tree a -> Tree a) -> Tree a
modifyRight' _mmzkTree _modifier = case _mmzkTree of
  Node {} -> _mmzkTree { right = right _mmzkTree `seq` _modifier (right _mmzkTree) }
  _       -> _mmzkTree

modifyValue' :: Tree a -> (a -> a) -> Tree a
modifyValue' _mmzkTree _modifier = case _mmzkTree of
  Leaf {} -> _mmzkTree { value = value _mmzkTree `seq` _modifier (value _mmzkTree) }
  Node {} -> _mmzkTree { value = value _mmzkTree `seq` _modifier (value _mmzkTree) }
  _       -> _mmzkTree

-- Boilerplates for List a.

isCons :: List a -> Bool
isCons Cons {} = True
isCons _       = False

isNil :: List a -> Bool
isNil Nil {} = True
isNil _      = False

getElement :: List a -> Maybe (a)
getElement _mmzkList = case _mmzkList of
  Cons {} -> Just $ element _mmzkList
  _       -> Nothing

getNext :: List a -> Maybe (List a)
getNext _mmzkList = case _mmzkList of
  Cons {} -> Just $ next _mmzkList
  _       -> Nothing

setElement :: List a -> a -> List a
setElement _mmzkList _element = case _mmzkList of
  Cons {} -> _mmzkList { element = _element }
  _       -> _mmzkList

setNext :: List a -> List a -> List a
setNext _mmzkList _next = case _mmzkList of
  Cons {} -> _mmzkList { next = _next }
  _       -> _mmzkList

modifyElement :: List a -> (a -> a) -> List a
modifyElement _mmzkList _modifier = case _mmzkList of
  Cons {} -> _mmzkList { element = _modifier $ element _mmzkList }
  _       -> _mmzkList

modifyNext :: List a -> (List a -> List a) -> List a
modifyNext _mmzkList _modifier = case _mmzkList of
  Cons {} -> _mmzkList { next = _modifier $ next _mmzkList }
  _       -> _mmzkList

modifyElement' :: List a -> (a -> a) -> List a
modifyElement' _mmzkList _modifier = case _mmzkList of
  Cons {} -> _mmzkList { element = element _mmzkList `seq` _modifier (element _mmzkList) }
  _       -> _mmzkList

modifyNext' :: List a -> (List a -> List a) -> List a
modifyNext' _mmzkList _modifier = case _mmzkList of
  Cons {} -> _mmzkList { next = next _mmzkList `seq` _modifier (next _mmzkList) }
  _       -> _mmzkList

-- Boilerplates for VBool.

isVBool :: VBool -> Bool
isVBool VBool {} = True
getBool :: VBool -> Maybe (Bool)

getBool _mmzkVBool = case _mmzkVBool of
  VBool {} -> Just $ bool _mmzkVBool

setBool :: VBool -> Bool -> VBool
setBool _mmzkVBool _bool = case _mmzkVBool of
  VBool {} -> _mmzkVBool { bool = _bool }

modifyBool :: VBool -> (Bool -> Bool) -> VBool
modifyBool _mmzkVBool _modifier = case _mmzkVBool of
  VBool {} -> _mmzkVBool { bool = _modifier $ bool _mmzkVBool }

modifyBool' :: VBool -> (Bool -> Bool) -> VBool
modifyBool' _mmzkVBool _modifier = case _mmzkVBool of
  VBool {} -> _mmzkVBool { bool = bool _mmzkVBool `seq` _modifier (bool _mmzkVBool) }
