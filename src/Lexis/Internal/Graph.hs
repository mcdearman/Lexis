module Lexis.Internal.Graph where

data Graph a = Graph
  { nodes :: [a],
    edges :: [(a, a)]
  }
  deriving (Show, Eq)