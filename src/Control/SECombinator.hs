-- | Semantic Editor Combinators
-- from Conal's
-- http://conal.net/blog/posts/semantic-editor-combinators/

module Control.SECombinator (
  argument, result
  ) where

result :: (a -> b) -> (c -> a) -> (c -> b)
result = (.)

argument :: (a -> b) -> (b -> c) -> (a -> c)
argument = flip (.)

