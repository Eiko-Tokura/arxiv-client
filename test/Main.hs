{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text as T
import Arxiv.Query
import Arxiv.Client
import Data.Function

failWith :: String -> IO a
failWith msg = ioError (userError msg)

assert :: Bool -> String -> IO ()
assert True _  = pure ()
assert False m = failWith m

main :: IO ()
main = do
  putStrLn "[test] renderSearchQuery builds fielded terms"
  let q = emptyQuery & inCategory "math.NT" & anyWords ["Chabauty","Coleman"]
  let rq = renderSearchQuery q
  putStrLn (" search_query = " <> T.unpack rq)
  assert ("cat:math.NT" `T.isInfixOf` rq) "missing cat:math.NT"
  assert ("all:" `T.isInfixOf` rq)        "missing 'all:' prefix"

  putStrLn "[test] simple network call returns some entries"
  es <- queryArxivIO (emptyQuery & anyWords ["electron"] & setPaging 0 5)
  assert (not (null es)) "expected non-empty result for 'electron'"

  putStrLn "[test] OK"
