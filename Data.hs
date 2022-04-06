module Data where

import           Data.Map (Map)

data Data = Data Int String (Map String (Map String String))
  deriving (Eq, Ord, Show)
