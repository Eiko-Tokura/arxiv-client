{-# LANGUAGE OverloadedStrings #-}

module Network.Arxiv.Query
  ( SortBy(..)
  , SortOrder(..)
  , ArxivQuery(..)
  , emptyQuery
  , setPaging
  , setSort
  , byAuthor
  , titleHas
  , abstractHas
  , inCategory
  , anyWords
  , allWords
  , exactPhrase
  , notWords
  , rawTerm
  , renderSearchQuery
  ) where

import Data.Text (Text)
import qualified Data.Text as T

data SortBy = Relevance | LastUpdatedDate | SubmittedDate
  deriving (Eq, Show)

data SortOrder = Asc | Desc
  deriving (Eq, Show)

-- | A minimal query model. We keep terms as fielded snippets that the arXiv
-- API understands, and render to the 'search_query' string.
data ArxivQuery = ArxivQuery
  { qTerms     :: [Text]           -- ^ fielded terms like \"ti:\\\"Haskell\\\"\", \"au:Einstein\"
  , qStart     :: !Int             -- ^ start index (offset)
  , qMax       :: !Int             -- ^ max results, up to ~300 per arXiv docs
  , qSortBy    :: !(Maybe SortBy)
  , qSortOrder :: !(Maybe SortOrder)
  , qIdList    :: ![Text]          -- ^ explicit id_list override (comma-separated)
  } deriving (Eq, Show)

emptyQuery :: ArxivQuery
emptyQuery = ArxivQuery [] 0 50 Nothing Nothing []

setPaging :: Int -> Int -> ArxivQuery -> ArxivQuery
setPaging s m q = q { qStart = max 0 s, qMax = max 1 m }

setSort :: SortBy -> SortOrder -> ArxivQuery -> ArxivQuery
setSort sb so q = q { qSortBy = Just sb, qSortOrder = Just so }

-- field helpers
field :: Text -> Text -> Text
field f v = f <> ":" <> quoteIfNeeded v

quoteIfNeeded :: Text -> Text
quoteIfNeeded t | T.any isSpecial t = T.concat ["\"", t, "\""]
                | otherwise         = t
  where
    isSpecial c = c == ' ' || c == ':' || c == '(' || c == ')' || c == '"'

byAuthor :: Text -> ArxivQuery -> ArxivQuery
byAuthor a q = q { qTerms = qTerms q <> [field "au" a] }

titleHas :: Text -> ArxivQuery -> ArxivQuery
titleHas t q = q { qTerms = qTerms q <> [field "ti" t] }

abstractHas :: Text -> ArxivQuery -> ArxivQuery
abstractHas t q = q { qTerms = qTerms q <> [field "abs" t] }

inCategory :: Text -> ArxivQuery -> ArxivQuery
inCategory c q = q { qTerms = qTerms q <> [field "cat" c] }

-- | Any of these words (OR).
anyWords :: [Text] -> ArxivQuery -> ArxivQuery
anyWords ws q
  | null ws   = q
  | otherwise =
      let one w = "all:" <> quoteIfNeeded w
      in q { qTerms = qTerms q <> [paren (T.intercalate "+OR+" (map one ws))] }

-- | All of these words (AND).
allWords :: [Text] -> ArxivQuery -> ArxivQuery
allWords ws q
  | null ws   = q
  | otherwise =
      let one w = "all:" <> quoteIfNeeded w
      in q { qTerms = qTerms q <> [paren (T.intercalate "+AND+" (map one ws))] }

-- | Exact phrase (double-quoted).
exactPhrase :: Text -> ArxivQuery -> ArxivQuery
exactPhrase p q = q { qTerms = qTerms q <> ["all:" <> "\"" <> p <> "\""] }

-- | Negate (NOT) these words.
notWords :: [Text] -> ArxivQuery -> ArxivQuery
notWords ws q
  | null ws   = q
  | otherwise =
      let one w = "-all:" <> quoteIfNeeded w
      in q { qTerms = qTerms q <> [T.intercalate "+AND+" (map one ws)] }

-- | Inject a raw term snippet (escape yourself).
rawTerm :: Text -> ArxivQuery -> ArxivQuery
rawTerm t q = q { qTerms = qTerms q <> [t] }

paren :: Text -> Text
paren t = "(" <> t <> ")"

-- | Final text for the API's search_query parameter.
renderSearchQuery :: ArxivQuery -> Text
renderSearchQuery q =
  if null (qTerms q) then "all:*" else T.intercalate "+AND+" (qTerms q)
