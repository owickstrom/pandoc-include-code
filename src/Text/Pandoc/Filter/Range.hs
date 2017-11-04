-- | A range that cannot be constructed with incorrect bounds.
module Text.Pandoc.Filter.Range (Range, mkRange) where

data Range = Range Int Int

mkRange :: Int -> Int -> Maybe Range
mkRange s e
  | s > 0 && e > 0 && s <= e = Just (Range s e)
  | otherwise = Nothing
