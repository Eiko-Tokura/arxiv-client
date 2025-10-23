-- | Module      : Arxiv.Query.Algebraic
-- Description : Algebraic representation of arXiv queries, can be useful for reading and constructing queries.
{-# LANGUAGE OverloadedStrings #-}
module Arxiv.Query.Algebraic where

import Data.Text (Text)
import Arxiv.Query

data IsHasAnyAll a
  = Is  !a
  | Has !a
  | Any ![a]
  | All ![a]
  deriving (Show, Eq, Read)

data QueryTerm
  = Title     !(IsHasAnyAll Text)
  | Abstract  !(IsHasAnyAll Text)
  | Author    !(IsHasAnyAll Text)
  | Category  !(IsHasAnyAll Text)
  | AnyWhere  !(IsHasAnyAll Text)
  | Or        !QueryTerm          !QueryTerm
  | And       !QueryTerm          !QueryTerm
  | Ors       ![QueryTerm]
  | Ands      ![QueryTerm]
  | Not       !QueryTerm
  deriving (Show, Eq, Read)

toTerms :: QueryTerm -> [Term]
toTerms (Title (Is t))      = [TField "ti" $ "\"" <> t <> "\""]
toTerms (Title (Has t))     = [TField "ti" t]
toTerms (Title (Any ts))    = [TOr  $ TField "ti" <$> ts]
toTerms (Title (All ts))    = [TAnd $ TField "ti" <$> ts]
toTerms (Abstract (Is t))   = [TField "abs" t]
toTerms (Abstract (Has t))  = [TField "abs" t]
toTerms (Abstract (Any ts)) = [TOr  $ TField "abs" <$> ts]
toTerms (Abstract (All ts)) = [TAnd $ TField "abs" <$> ts]
toTerms (Author (Is t))     = [TField "au" $ "\"" <> t <> "\""]
toTerms (Author (Has t))    = [TField "au" t]
toTerms (Author (Any ts))   = [TOr  $ TField "au" <$> ts]
toTerms (Author (All ts))   = [TAnd $ TField "au" <$> ts]
toTerms (Category (Is t))   = [TField "cat" t]
toTerms (Category (Has t))  = [TField "cat" t]
toTerms (Category (Any ts)) = [TOr  $ TField "cat" <$> ts]
toTerms (Category (All ts)) = [TAnd $ TField "cat" <$> ts]
toTerms (AnyWhere (Is t))   = [TField "all" t]
toTerms (AnyWhere (Has t))  = [TField "all" t]
toTerms (AnyWhere (Any ts)) = [TOr  $ TField "all" <$> ts]
toTerms (AnyWhere (All ts)) = [TAnd $ TField "all" <$> ts]
toTerms (Or t1 t2)          = [TOr (toTerms t1 ++ toTerms t2)]
toTerms (Ors ts)            = [TOr (concatMap toTerms ts)]
toTerms (And t1 t2)         = [TAnd (toTerms t1 ++ toTerms t2)]
toTerms (Ands ts)           = [TAnd (concatMap toTerms ts)]
toTerms (Not t)             = [TNot (TAnd $ toTerms t)]
{-# INLINE toTerms #-}

applyQueryTerm :: QueryTerm -> ArxivQuery -> ArxivQuery
applyQueryTerm qt query = foldr addTerm query ( toTerms qt )
{-# INLINE applyQueryTerm #-}
