module ConstraintParser where

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec

import SmallInt
import SmallIntConstraints

parseConstraintFile :: String -> IO (Either String [SmallIntConstraint])
parseConstraintFile fileName =
  either (return . Left . show) (return . Right) =<< parseFromFile pConstraintFile fileName

pConstraintFile = topLevel (many pConstraint)

pConstraint =
  do l <- pArg
     symbol "<="
     r <- pArg
     return (LTE l r)
     
pArg = Left `liftM` pIdent <|> Right `liftM` pSmallInt

pIdent = lexeme $ do
  f <- letter
  r <- many alphaNum
  return (f:r)

pSmallInt = lexeme (many1 digit >>= (either fail return . safeFromInt . read))

symbol = lexeme . string
lexeme p = do r <- p; spaces; return r
topLevel p = do spaces; r <- p; eof; return r