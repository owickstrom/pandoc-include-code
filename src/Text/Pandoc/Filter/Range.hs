-- | A range that cannot be constructed with incorrect bounds.
module Text.Pandoc.Filter.Range
  ( Range
  , rangeStart
  , rangeEnd
  , mkRange
  ) where

data Range = Range { rangeStart :: Int
                   , rangeEnd   :: Int
                   }
  deriving (Show, Eq)

mkRange :: Int -> Int -> Maybe Range
mkRange s e
  | s > 0 && e > 0 && s <= e = Just (Range s e)
  | otherwise = Nothing
