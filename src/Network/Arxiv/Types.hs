{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Arxiv.Types
  ( ArxivEntry(..)
  , arxivIdNoVersion
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Parsed arXiv entry from the Atom feed.
data ArxivEntry = ArxivEntry
  { arxivId       :: !Text       -- ^ e.g. \"2401.01234v2\"
  , absUrl        :: !Text       -- ^ https://arxiv.org/abs/<id>
  , pdfUrl        :: !Text       -- ^ https://arxiv.org/pdf/<id>.pdf
  , sourceUrl     :: !Text       -- ^ https://arxiv.org/src/<id> (tarball of sources)
  , title         :: !Text
  , summary       :: !Text       -- ^ abstract
  , authors       :: ![Text]     -- ^ author names
  , categories    :: ![Text]     -- ^ arXiv subjects, e.g. \"math.NT\"
  , published     :: !UTCTime
  , updated       :: !UTCTime
  } deriving (Show, Eq, Generic)

-- | Drop a trailing version suffix (…vN) if present.
arxivIdNoVersion :: Text -> Text
arxivIdNoVersion t = case T.breakOnEnd "v" t of
  (prefix, ver) | not (T.null prefix) && T.all (`elem` ['0'..'9']) ver -> T.dropEnd (T.length ver + 1) t
  _ -> T.takeWhile (/= 'v') t
