module Network.Arxiv.Filters
  ( publishedBetween
  , publishedAfter
  , publishedBefore
  , hasCategory
  , authorContains
  , titleContains
  , abstractContains
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Network.Arxiv.Types

publishedBetween :: UTCTime -> UTCTime -> [ArxivEntry] -> [ArxivEntry]
publishedBetween a b = filter (\e -> published e >= a && published e <= b)

publishedAfter :: UTCTime -> [ArxivEntry] -> [ArxivEntry]
publishedAfter t = filter (\e -> published e >= t)

publishedBefore :: UTCTime -> [ArxivEntry] -> [ArxivEntry]
publishedBefore t = filter (\e -> published e <= t)

hasCategory :: Text -> [ArxivEntry] -> [ArxivEntry]
hasCategory cat = filter (\e -> cat `elem` categories e)

authorContains :: Text -> [ArxivEntry] -> [ArxivEntry]
authorContains needle =
  let n = T.toCaseFold needle
  in filter (any (T.isInfixOf n . T.toCaseFold) . authors)

titleContains :: Text -> [ArxivEntry] -> [ArxivEntry]
titleContains needle =
  let n = T.toCaseFold needle
  in filter (\e -> n `T.isInfixOf` T.toCaseFold (title e))

abstractContains :: Text -> [ArxivEntry] -> [ArxivEntry]
abstractContains needle =
  let n = T.toCaseFold needle
  in filter (\e -> n `T.isInfixOf` T.toCaseFold (summary e))
