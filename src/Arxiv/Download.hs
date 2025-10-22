-- | Convenient functions for downloading arXiv papers.
module Arxiv.Download where

import Arxiv.Entry
import Control.Monad
import Network.HTTP.Req
import Text.URI (mkURI)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)

downloadPdf :: ArxivEntry -> IO BL.ByteString
downloadPdf entry = runReq defaultHttpConfig $ do
  let url = pdfUrl entry
  uri <- mkURI url
  case useURI uri of
    Just (Left (httpUrl, options)) -> do
      response <- req GET httpUrl NoReqBody lbsResponse options
      return (responseBody response)
    Just (Right (httpsUrl, options)) -> do
      response <- req GET httpsUrl NoReqBody lbsResponse options
      return (responseBody response)
    Nothing -> error $ "Invalid URL: " ++ T.unpack url

-- | Usage:
-- @defaultFileName ".pdf" entry@
defaultFileName :: String -> ArxivEntry -> FilePath
defaultFileName suffix entry = toValidFileName (T.unpack (title entry)) ++ "-" ++ T.unpack (arxivId entry) ++ suffix

toValidFileName :: String -> String
toValidFileName = concatMap replaceInvalid
  where
    replaceInvalid c
      | c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9'] = [c]
      | c `elem` " ._-" = [c]
      | otherwise = "_"

-- | Detect if the file is already exists before downloading.
downloadPdfToFile :: ArxivEntry -> FilePath -> IO ()
downloadPdfToFile entry filePath = do
  fileExists <- doesFileExist filePath
  unless fileExists $ do
      pdfData <- downloadPdf entry
      BL.writeFile filePath pdfData

downloadSource :: ArxivEntry -> IO BL.ByteString
downloadSource entry = runReq defaultHttpConfig $ do
  let url = sourceUrl entry
  uri <- mkURI url
  case useURI uri of
    Just (Left (httpUrl, options)) -> do
      response <- req GET httpUrl NoReqBody lbsResponse options
      return (responseBody response)
    Just (Right (httpsUrl, options)) -> do
      response <- req GET httpsUrl NoReqBody lbsResponse options
      return (responseBody response)
    Nothing -> error $ "Invalid URL: " ++ T.unpack url

downloadSourceToFile :: ArxivEntry -> FilePath -> IO ()
downloadSourceToFile entry filePath = do
  fileExists <- doesFileExist filePath
  unless fileExists $ do
      srcData <- downloadSource entry
      BL.writeFile filePath srcData
