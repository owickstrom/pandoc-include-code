-- | A range that cannot be constructed with incorrect bounds.
module Text.Pandoc.Filter.Range
  ( LineNumber
  , Range
  , rangeStart
  , rangeEnd
  , mkRange
  ) where

type LineNumber = Int

data Range = Range { rangeStart :: LineNumber
                   , rangeEnd   :: LineNumber
                   }
  deriving (Show, Eq)

mkRange :: LineNumber -> LineNumber -> Maybe Range
mkRange s e
  | s > 0 && e > 0 && s <= e = Just (Range s e)
  | otherwise = Nothing
