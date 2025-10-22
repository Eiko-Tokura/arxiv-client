{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Arxiv.Query
Description : A tiny, composable DSL for arXiv API search queries
Copyright   : (c) You
License     : BSD3
Maintainer  : you@example.com
Stability   : experimental
Portability : portable

This module provides a small, ergonomic DSL to compose arXiv queries that
render to the API's @search_query@ parameter. It also carries paging and
sorting settings, plus an optional @id_list@ (server-side filtering by IDs).

### TL;DR

Build queries fluently with /builders/ (functions of type 'QBuilder'):

@
import Data.Function ((&))
import Network.Arxiv.Query

let q =
      emptyQuery
      & ( titleAll [\"Coleman\",\"Tropical\"]
          .|| title \"Chabauty\"
          .&& (title \"Differential\" .|| title \"Equation\")
        )
      .&& inCategory \"math.NT\"
      & setSort SubmittedDate Desc
      & setPaging 0 25

renderSearchQuery q
-- (ti:Coleman+AND+ti:Tropical)+OR+(ti:Chabauty)+AND+((ti:Differential)+OR+(ti:Equation))+AND+cat:math.NT
@

- '(.&&)' and '(.||)' are boolean-style combinators with sensible precedence:
  @(.&&)@ binds tighter than @(.||)@ (like '&&' vs '||' in Haskell).
- Builders keep parentheses correct using an internal AST.

### Fields & Syntax

arXiv supports fielded search terms such as:

* @ti:@ (title), @abs:@ (abstract), @au:@ (author), @cat:@ (category)
* @co:@ (comment), @jr:@ (journal reference), @rn:@ (report number)
* @all:@ searches all fields (used by 'anyWords', 'allWords', 'exactPhrase')

Values are automatically quoted when they contain spaces or special characters.
You can also inject raw snippets via 'rawTerm' / 'rawGroupAnd' / 'rawGroupOr'
when you want full control.

### Paging, Sorting, and id_list

- Use 'setPaging' to set @start@ and @max_results@ (arXiv typically allows up
  to ~300).
- Use 'setSort' to choose 'SortBy' and 'SortOrder'.
- 'setIdList' / 'addId' / 'clearIdList' manipulate the optional @id_list@
  parameter (comma-separated), which the client layer will send alongside
  @search_query@.

@renderSearchQuery@ only renders the query expression; paging, sorting, and
@id_list@ are carried in 'ArxivQuery' and used by the client code.

### Not / Negation

- To negate words across all fields, use 'notWords'.
- To negate field-specific predicates, use 'notInTitle', 'notInAbstract',
  'notInCategories', or the general 'groupNot'.

@since 0.1.0.0
-}
module Network.Arxiv.Query
  ( -- * Sorting
    SortBy(..)
  , SortOrder(..)

    -- * Query record & basics
  , ArxivQuery(..)
  , emptyQuery
  , setPaging
  , setSort

    -- * Author predicates
  , byAuthor
  , authorAny
  , authorAll
  , authorExact

    -- * Title predicates
  , titleHas
  , titleAny
  , titleAll
  , titlePhrase
  , notInTitle

    -- * Abstract predicates
  , abstractHas
  , abstractAny
  , abstractAll
  , abstractPhrase
  , notInAbstract

    -- * Category predicates
  , inCategory
  , categoriesAny
  , categoriesAll
  , notInCategories

    -- * All-fields helpers
  , anyWords
  , allWords
  , exactPhrase
  , notWords

    -- * Raw injections
  , rawTerm
  , rawGroupAnd
  , rawGroupOr

    -- * Boolean-style combinators and grouping
  , (.&&)
  , (.||)
  , groupAnd
  , groupOr
  , groupNot

    -- * Other fields
  , commentHas
  , journalRefHas
  , reportNumberIs

    -- * id_list helpers
  , setIdList
  , addId
  , clearIdList

    -- * Rendering
  , renderSearchQuery
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Builder type for arXiv queries.
--
-- A 'QBuilder' takes an 'ArxivQuery' and returns a modified one. Most
-- functions in this module are such builders and can be composed with
-- function application or with '(.&&)' / '(.||)'.
type QBuilder = ArxivQuery -> ArxivQuery

-- Sorting ---------------------------------------------------------------

-- | Sorting key for the arXiv API.
data SortBy
  = Relevance         -- ^ @sortBy=relevance@
  | LastUpdatedDate   -- ^ @sortBy=lastUpdatedDate@
  | SubmittedDate     -- ^ @sortBy=submittedDate@
  deriving (Eq, Show)

-- | Sort direction.
data SortOrder = Asc | Desc
  deriving (Eq, Show)

-- Tiny internal AST -----------------------------------------------------

-- | Internal query AST (keeps parentheses and negations correct).
data Term
  = TField Text Text            -- ^ @f:v@  (auto-quoting values as needed)
  | TAllWord Text               -- ^ @all:v@
  | TAllPhrase Text             -- ^ @all:\"...\"@
  | TRaw Text                   -- ^ Injected verbatim snippet
  | TNot Term
  | TAnd [Term]
  | TOr  [Term]
  deriving (Eq, Show)

-- Query record ----------------------------------------------------------

-- | The full query configuration that the client consumes.
data ArxivQuery = ArxivQuery
  { qTerms     :: [Term]         -- ^ Query expression terms (top-level AND).
  , qStart     :: !Int           -- ^ Paging start index (offset).
  , qMax       :: !Int           -- ^ Paging size (max_results).
  , qSortBy    :: !(Maybe SortBy)    -- ^ Optional sort key.
  , qSortOrder :: !(Maybe SortOrder) -- ^ Optional sort direction.
  , qIdList    :: ![Text]        -- ^ Optional @id_list@ (comma-separated).
  } deriving (Eq, Show)

-- | An empty query with no terms, start @0@, max @50@, and no sorting.
emptyQuery :: ArxivQuery
emptyQuery = ArxivQuery [] 0 50 Nothing Nothing []

-- | Set paging parameters.
--
-- Values are clamped to @start >= 0@ and @max >= 1@.
setPaging :: Int -> Int -> QBuilder
setPaging s m q = q { qStart = max 0 s, qMax = max 1 m }

-- | Set sorting: see 'SortBy' and 'SortOrder'.
setSort :: SortBy -> SortOrder -> QBuilder
setSort sb so q = q { qSortBy = Just sb, qSortOrder = Just so }

-- Helpers ---------------------------------------------------------------

quoteIfNeeded :: Text -> Text
quoteIfNeeded t
  | T.any (\c -> c == ' ' || c == ':' || c == '(' || c == ')' || c == '"') t
      = "\"" <> t <> "\""
  | otherwise = t

paren :: Text -> Text
paren x = "(" <> x <> ")"

renderTerm :: Term -> Text
renderTerm (TField f v)   = f <> ":" <> quoteIfNeeded v
renderTerm (TAllWord w)   = "all:"   <> quoteIfNeeded w
renderTerm (TAllPhrase p) = "all:"   <> "\"" <> p <> "\""
renderTerm (TRaw t)       = t
renderTerm (TNot t)       = "-" <> renderTerm t
renderTerm (TAnd xs)      = paren (T.intercalate " AND " (map renderTerm xs))
renderTerm (TOr  xs)      = paren (T.intercalate " OR "  (map renderTerm xs))

addTerm :: Term -> QBuilder
addTerm t q = q { qTerms = qTerms q <> [t] }

-- Existing API (preserved) ---------------------------------------------

-- | Search by author: @au:&lt;author&gt;@.
--
-- Examples:
--
-- @
-- emptyQuery & byAuthor "Minhyong Kim"
-- emptyQuery & authorExact "Minhyong Kim"  -- quoted exact match
-- @
byAuthor :: Text -> QBuilder
byAuthor a = addTerm (TField "au" a)

-- | Title contains a token: @ti:&lt;title&gt;@.
titleHas :: Text -> QBuilder
titleHas t = addTerm (TField "ti" t)

-- | Abstract contains a token: @abs:&lt;abstract&gt;@.
abstractHas :: Text -> QBuilder
abstractHas t = addTerm (TField "abs" t)

-- | In category: @cat:&lt;category&gt;@ (e.g. @\"math.NT\"@).
inCategory :: Text -> QBuilder
inCategory c = addTerm (TField "cat" c)

-- | Match any of the given words across all fields (OR): @all:…@.
anyWords :: [Text] -> QBuilder
anyWords []  q = q
anyWords ws  q = addTerm (TOr  (map TAllWord ws)) q

-- | Match all the given words across all fields (AND): @all:…@.
allWords :: [Text] -> QBuilder
allWords []  q = q
allWords ws  q = addTerm (TAnd (map TAllWord ws)) q

-- | Match an exact phrase across all fields: @all:\"…\"@.
exactPhrase :: Text -> QBuilder
exactPhrase p = addTerm (TAllPhrase p)

-- | Negate words across all fields: @-all:…@.
notWords :: [Text] -> QBuilder
notWords []  q = q
notWords ws  q = addTerm (TAnd (map (TNot . TAllWord) ws)) q

-- | Inject a raw term snippet exactly as written.
--
-- Use this when you need complete control over the fielding/quoting.
rawTerm :: Text -> QBuilder
rawTerm t = addTerm (TRaw t)

-- New fielded combinators ----------------------------------------------

-- | Title contains /any/ of these tokens: @(ti:a OR ti:b …)@.
titleAny :: [Text] -> QBuilder
titleAny ws = addTerm (TOr  (map (TField "ti") ws))

-- | Title contains /all/ of these tokens: @(ti:a AND ti:b …)@.
titleAll :: [Text] -> QBuilder
titleAll ws = addTerm (TAnd (map (TField "ti") ws))

-- | Title contains this exact phrase: @ti:\"…\"@.
titlePhrase :: Text -> QBuilder
titlePhrase p = addTerm (TRaw ("ti:" <> "\"" <> p <> "\""))

-- | Title does /not/ contain any of these tokens: @-ti:a AND -ti:b …@.
notInTitle :: [Text] -> QBuilder
notInTitle ws = addTerm (TAnd (map (TNot . TField "ti") ws))

-- | Abstract contains /any/ of these tokens: @(abs:a OR abs:b …)@.
abstractAny :: [Text] -> QBuilder
abstractAny ws = addTerm (TOr  (map (TField "abs") ws))

-- | Abstract contains /all/ of these tokens: @(abs:a AND abs:b …)@.
abstractAll :: [Text] -> QBuilder
abstractAll ws = addTerm (TAnd (map (TField "abs") ws))

-- | Abstract contains this exact phrase: @abs:\"…\"@.
abstractPhrase :: Text -> QBuilder
abstractPhrase p = addTerm (TRaw ("abs:" <> "\"" <> p <> "\""))

-- | Abstract does /not/ contain any of these tokens.
notInAbstract :: [Text] -> QBuilder
notInAbstract ws = addTerm (TAnd (map (TNot . TField "abs") ws))

-- | Author matches /any/ token: @(au:a OR au:b …)@.
authorAny :: [Text] -> QBuilder
authorAny ws = addTerm (TOr  (map (TField "au") ws))

-- | Author matches /all/ tokens: @(au:a AND au:b …)@.
authorAll :: [Text] -> QBuilder
authorAll ws = addTerm (TAnd (map (TField "au") ws))

-- | Author exact (quoted): @au:\"…\"@.
authorExact :: Text -> QBuilder
authorExact a = addTerm (TRaw ("au:" <> "\"" <> a <> "\""))

-- | Category is /any/ of: @(cat:a OR cat:b …)@.
categoriesAny :: [Text] -> QBuilder
categoriesAny cs = addTerm (TOr  (map (TField "cat") cs))

-- | Category is /all/ of: @(cat:a AND cat:b …)@.
categoriesAll :: [Text] -> QBuilder
categoriesAll cs = addTerm (TAnd (map (TField "cat") cs))

-- | Category is /not/ any of: @-cat:a AND -cat:b …@.
notInCategories :: [Text] -> QBuilder
notInCategories cs = addTerm (TAnd (map (TNot . TField "cat") cs))

-- | Comment contains a token: @co:&lt;text&gt;@.
commentHas :: Text -> QBuilder
commentHas t = addTerm (TField "co" t)

-- | Journal reference contains a token: @jr:&lt;text&gt;@.
journalRefHas :: Text -> QBuilder
journalRefHas t = addTerm (TField "jr" t)

-- | Report number equals token (usually exact): @rn:&lt;id&gt;@.
reportNumberIs :: Text -> QBuilder
reportNumberIs rn = addTerm (TField "rn" rn)

-- Boolean-style combinators & grouping ---------------------------------

infixr 3 .&&   -- | AND; binds tighter (like '&&').
infixr 2 .||   -- | OR; binds looser (like '||').

-- | Combine two builders under an AND group (parenthesized).
--
-- @
-- title \"Chabauty\" .&& inCategory \"math.NT\"
-- -- renders as: (ti:Chabauty)+AND+cat:math.NT
-- @
(.&&) :: QBuilder -> QBuilder -> QBuilder
(.&&) f g = groupAnd [f, g]

-- | Combine two builders under an OR group (parenthesized).
(.||) :: QBuilder -> QBuilder -> QBuilder
(.||) f g = groupOr  [f, g]

-- | Group a list of builders using AND: @(A AND B AND …)@.
groupAnd :: [QBuilder] -> QBuilder
groupAnd fs q0 =
  let ts = qTerms (foldl (\q f -> f q) (q0 { qTerms = [] }) fs)
  in addTerm (TAnd ts) q0

-- | Group a list of builders using OR: @(A OR B OR …)@.
groupOr :: [QBuilder] -> QBuilder
groupOr fs q0 =
  let ts = qTerms (foldl (\q f -> f q) (q0 { qTerms = [] }) fs)
  in addTerm (TOr ts) q0

-- | Negate a grouped builder: @-(A AND B …)@.
--
-- If the builder expands to multiple terms, they are AND-ed before negation.
groupNot :: QBuilder -> QBuilder
groupNot f q0 =
  let ts = qTerms (f (q0 { qTerms = [] }))
  in case ts of
       []    -> q0
       [one] -> addTerm (TNot one) q0
       many  -> addTerm (TNot (TAnd many)) q0

-- Raw groups (inject ready-made snippets) -------------------------------

-- | Group raw snippets with AND, injected verbatim.
rawGroupAnd :: [Text] -> QBuilder
rawGroupAnd xs = addTerm (TAnd (map TRaw xs))

-- | Group raw snippets with OR, injected verbatim.
rawGroupOr :: [Text] -> QBuilder
rawGroupOr xs = addTerm (TOr (map TRaw xs))

-- id_list helpers -------------------------------------------------------

-- | Replace the entire @id_list@ (comma-separated on the wire).
--
-- This is sent alongside @search_query@ by the client layer; arXiv will
-- restrict results to those IDs.
setIdList :: [Text] -> QBuilder
setIdList ids q = q { qIdList = ids }

-- | Append a single ID to @id_list@.
addId :: Text -> QBuilder
addId i q = q { qIdList = qIdList q <> [i] }

-- | Clear @id_list@.
clearIdList :: QBuilder
clearIdList q = q { qIdList = [] }

-- Render ---------------------------------------------------------------

-- | Render the query expression (for the API's @search_query@ parameter).
--
-- Note that paging, sorting, and @id_list@ are not part of this string; they
-- are carried in 'ArxivQuery' and used by the HTTP client.
--
-- If no terms are present, renders to @all:*@.
renderSearchQuery :: ArxivQuery -> Text
renderSearchQuery q =
  if null (qTerms q)
     then "all:*"
     else T.intercalate " AND " (map renderTerm (qTerms q))
