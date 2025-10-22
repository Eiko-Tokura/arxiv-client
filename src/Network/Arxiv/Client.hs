{-# LANGUAGE OverloadedStrings, OverloadedRecordDot #-}

module Network.Arxiv.Client
  ( queryArxiv
  , queryArxivIO
  , queryArxivRaw
  , queryArxivRawIO
  , buildRequestUrlText
  , downloadTo
  ) where

import           Control.Applicative ((<|>), asum)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Maybe (listToMaybe, fromMaybe, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import           Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           Network.Arxiv.Query
import           Network.Arxiv.Types
import           Network.HTTP.Req
import qualified Text.XML as X
import           Text.XML.Cursor
import qualified Text.URI as URI

axisLocal :: Text -> Axis
axisLocal ln = element (nNamespace ln)

-- Identify ourselves per arXiv guidelines.
userAgentHeader :: Option scheme
userAgentHeader =
  header "User-Agent" "haskell-arxiv-client/0.1"

-- Build the query string for debugging (best-effort).
buildRequestUrlText :: ArxivQuery -> Text
buildRequestUrlText q =
  let base = "http://export.arxiv.org/api/query?"
      kv k v = k <> "=" <> v
      joinAmp = T.intercalate "&"
      parts =
        [ kv "search_query" (renderSearchQuery q)
        , kv "start"        (tshow (qStart q))
        , kv "max_results"  (tshow (qMax   q))
        ] <>
        maybe [] (\sb -> [kv "sortBy"    (sortByText sb)]) (qSortBy q) <>
        maybe [] (\so -> [kv "sortOrder" (sortOrderText so)]) (qSortOrder q) <>
        [kv "id_list" (T.intercalate "," (qIdList q)) | not (null (qIdList q))]
  in base <> joinAmp parts

-- Raw response (XML bytes) + parsed entries, for debugging.
queryArxivRaw :: (MonadHttp m, MonadIO m) => ArxivQuery -> m (LBS.ByteString, [ArxivEntry])
queryArxivRaw q = do
  let baseUrl = http "export.arxiv.org" /: "api" /: "query"
      mkParam name val = name =: (val :: Text)
      params =
           mkParam "search_query" (renderSearchQuery q)
        <> mkParam "start"       (tshow (qStart q))
        <> mkParam "max_results" (tshow (qMax   q))
        <> maybe mempty (\sb -> "sortBy"    =: sortByText sb) (qSortBy q)
        <> maybe mempty (\so -> "sortOrder" =: sortOrderText so) (qSortOrder q)
        <> (if null (qIdList q) then mempty else "id_list" =: T.intercalate "," (qIdList q))
        <> userAgentHeader
  r <- req GET baseUrl NoReqBody lbsResponse params
  let bs = responseBody r
  pure (bs, parseFeed bs)

-- queryArxiv now just calls queryArxivRaw and returns parsed entries.
queryArxiv :: (MonadHttp m, MonadIO m) => ArxivQuery -> m [ArxivEntry]
queryArxiv q = snd <$> queryArxivRaw q
-----------------------------

queryArxivRawIO :: ArxivQuery -> IO (LBS.ByteString, [ArxivEntry])
queryArxivRawIO q = runReq defaultHttpConfig (queryArxivRaw q)

-- | Run in IO.
queryArxivIO :: ArxivQuery -> IO [ArxivEntry]
queryArxivIO q = runReq defaultHttpConfig (queryArxiv q)

tshow :: Show a => a -> Text
tshow = T.pack . show

sortByText :: SortBy -> Text
sortByText Relevance       = "relevance"
sortByText LastUpdatedDate = "lastUpdatedDate"
sortByText SubmittedDate   = "submittedDate"

sortOrderText :: SortOrder -> Text
sortOrderText Asc  = "ascending"
sortOrderText Desc = "descending"

nNamespace :: Text -> X.Name
nNamespace local = X.Name local (Just "http://www.w3.org/2005/Atom") Nothing

nAttr :: Text -> X.Name
nAttr local = X.Name local Nothing Nothing

parseFeed :: LBS.ByteString -> [ArxivEntry]
parseFeed lbs =
  let doc     = X.parseLBS_ X.def lbs
      rootCur = fromDocument doc
      -- Descendant axis + local-name match (namespace-agnostic).
      entries = rootCur $// axisLocal "entry"
  in mapMaybe cursorToEntry entries

contents :: Cursor -> [Text]
contents cur = case cur.node of
  X.NodeContent t -> [t]
  _               -> case cur $/ content of
                       [] -> []
                       ts -> ts

-- | Safely take the first concatenated text of an element.
txtFirstOf :: Cursor -> Text -> Maybe Text
txtFirstOf cur local =
  case concatMap contents (cur $// axisLocal local) of
    [] -> Nothing
    xs -> Just (T.strip (T.concat xs))

cursorToEntry :: Cursor -> Maybe ArxivEntry
cursorToEntry e = do
  title'     <- txtFirstOf e "title"
  summary'   <- txtFirstOf e "summary"
  idURL      <- txtFirstOf e "id"
  published' <- txtFirstOf e "published" >>= parseTime
  updated'   <- txtFirstOf e "updated" >>= parseTime

  -- Authors: for each <author>, concat all <name> text nodes. Missing -> "".
  let authors' =
        [ T.strip . T.concat $ concatMap contents (aCur $/ laxElement "name")
        | aCur <- e $// axisLocal "author"
        ]

      cats =
        (e $// axisLocal "category") >>= attribute (nAttr "term")

      -- pick <link ... href="..."> by attribute match
      pickLink :: Text -> Text -> Maybe Text
      pickLink attrName attrVal =
        let links = e $// (axisLocal "link" >=> attributeIs (nAttr attrName) attrVal)
        in listToMaybe (links >>= attribute (nAttr "href"))

      arxId = lastSegment idURL

      absL = fromMaybe ("https://arxiv.org/abs/" <> arxId)
             (pickLink "rel" "alternate")

      pdfL = fromMaybe ("https://arxiv.org/pdf/" <> arxId <> ".pdf")
             ( pickLink "title" "pdf"
               <|> pickLink "type" "application/pdf"
             )

      srcL = "https://arxiv.org/src/" <> arxId

  pure ArxivEntry
        { arxivId   = arxId
        , absUrl    = absL
        , pdfUrl    = pdfL
        , sourceUrl = srcL
        , title     = title'
        , summary   = summary'
        , authors   = authors'
        , categories= cats
        , published = published'
        , updated   = updated'
        }

parseTime :: Text -> Maybe UTCTime
parseTime t = --iso8601ParseM . T.unpack
  let s = T.unpack (T.strip t)
  in asum
       [ iso8601ParseM s
       , parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"      s
       , parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"    s
       , parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Ez"    s
       , parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Ez"    s
       ]

lastSegment :: Text -> Text
lastSegment url =
  case reverse (T.splitOn "/" (T.takeWhile (/= '#') url)) of
    (x:_) | not (T.null x) -> x
    _                      -> url

-- | Download any http/https URL to a file, using 'modern-uri'.
downloadTo :: (MonadHttp m, MonadIO m) => Text -> FilePath -> m ()
downloadTo urlT fp =
  case URI.mkURI urlT of
    Left _err -> error "downloadTo: invalid URL"
    Right uri ->
      case useHttpsURI uri of
        Just (u, opt) -> do
          r <- req GET u NoReqBody lbsResponse opt
          liftIO (LBS.writeFile fp (responseBody r))
        Nothing ->
          case useHttpURI uri of
            Just (u, opt) -> do
              r <- req GET u NoReqBody lbsResponse opt
              liftIO (LBS.writeFile fp (responseBody r))
            Nothing ->
              error "downloadTo: unsupported scheme (expect http/https)"
