{-# LANGUAGE OverloadedStrings #-}
-- | Parse algebraic query expressions
module Arxiv.Query.Parser
  ( parseQueryTerm
  , queryTermParser
  , runQueryTermTests
  ) where

import Arxiv.Query.Algebraic
import Control.Applicative (asum)
import Control.Monad (forM_)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text.IO as TIO

{- example:

title is coleman     ---> Title (Is "coleman")
title has "coleman integral"  ---> Title (Has "coleman integral")
ands [<queryTerm>, ...]  ---> Ands [<queryTerm>, ...]

-}
queryTermParser :: Parsec Void Text QueryTerm
queryTermParser = asum
  [ titleParser
  , abstractParser
  , authorParser
  , categoryParser
  , anywhereParser
  , fmap Ands $ string "ands" >> space >> listOf queryTermParser
  , fmap Ors  $ string "ors"  >> space >> listOf queryTermParser
  ]
  where
    titleParser    = string "title"    >> space1 >> Title    <$> isHasAnyAllParser
    abstractParser = string "abstract" >> space1 >> Abstract <$> isHasAnyAllParser
    authorParser   = string "author"   >> space1 >> Author   <$> isHasAnyAllParser
    categoryParser = string "category" >> space1 >> Category <$> isHasAnyAllParser
    anywhereParser = string "anywhere" >> space1 >> AnyWhere <$> isHasAnyAllParser
    isHasAnyAllParser = asum
      [ Is  <$> (string "is"  >> space1 >> quotedTextParser)
      , Has <$> (string "has" >> space1 >> quotedTextParser)
      , Any <$> (string "any" >> space1 >> listOf quotedTextParser)
      , All <$> (string "all" >> space1 >> listOf quotedTextParser)
      ]
    quotedTextParser = pack <$> (char '"' >> manyTill anySingle (char '"'))
    listOf p         = between (char '[') (char ']') (sepBy p (char ',' >> space))

parseQueryTerm :: Text -> Either (ParseErrorBundle Text Void) QueryTerm
parseQueryTerm = runParser (queryTermParser <* eof) ""

queryTermTestItem :: Text -> QueryTerm -> Maybe String
queryTermTestItem input expected =
  case runParser (queryTermParser <* eof) "" input of
    Left err    -> Just ("Parse error: " ++ errorBundlePretty err)
    Right actual ->
      if actual == expected
        then Nothing
        else Just ("Expected: " ++ show expected ++ ", but got: " ++ show actual)

queryTermTests :: [(Text, QueryTerm)]
queryTermTests =
  [ ("title is \"coleman\"", Title (Is "coleman"))
  , ("abstract has \"p-adic integral\"", Abstract (Has "p-adic integral"))
  , ("author any [\"john doe\", \"jane smith\"]", Author (Any ["john doe", "jane smith"]))
  , ("category all [\"math.NT\", \"math.AG\"]", Category (All ["math.NT", "math.AG"]))
  , ("anywhere is \"quantum mechanics\"", AnyWhere (Is "quantum mechanics"))
  , ("ands [title is \"coleman\", author has \"doe\"]", Ands [Title (Is "coleman"), Author (Has "doe")])
  , ("ors [category is \"math.NT\", category is \"math.AG\"]", Ors [Category (Is "math.NT"), Category (Is "math.AG")])
  ]

runQueryTermTests :: IO ()
runQueryTermTests = do
  let results = map (\(input, expected) -> (input, queryTermTestItem input expected)) queryTermTests
  let failures = [(input, err) | (input, Just err) <- results]
  if null failures
    then putStrLn "All query term parser tests passed."
    else do
      putStrLn "Some query term parser tests failed:"
      forM_ failures $ \(input, err) -> do
        putStr "Input: " >> TIO.putStr input
        putStrLn $ "\nError: " ++ err
