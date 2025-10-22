{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Time (UTCTime(..), secondsToDiffTime, fromGregorian)
import Network.Arxiv.Client
import Network.Arxiv.Query
import Network.Arxiv.Filters
import Network.Arxiv.Types
import Text.Pretty.Simple

fromDay :: Integer -> Int -> Int -> UTCTime
fromDay y m d = UTCTime (fromGregorian y m d) (secondsToDiffTime 0)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

main :: IO ()
main = do
  let q0 = emptyQuery
         & inCategory "math.NT"
         & titleHas "Chabauty"
         & setPaging 0 1
         & setSort SubmittedDate Desc

  putStrLn "----- Query debug -----"
  T.putStrLn ("search_query = " <> renderSearchQuery q0)
  T.putStrLn ("full URL     = " <> buildRequestUrlText q0)

  (raw, es) <- queryArxivRawIO q0
  putStrLn "----- Raw response -----"
  BL8.putStrLn raw

  putStrLn "----- Parsed entries -----"
  putStrLn ("Total parsed: " <> show (length es))
  mapM_ (\e -> T.putStrLn ("• " <> title e)) (take 5 es)

  let recent = publishedAfter (fromDay 2022 01 01) es
  putStrLn ("Recent (>=2022): " <> show (length recent))
  mapM_ pPrint recent

  if null recent
    then putStrLn "No recent papers found."
    else putStrLn $ "Total recent papers found: " <> show (length recent)
