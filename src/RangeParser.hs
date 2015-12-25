module RangeParser (parseRange) where

import Text.Printf
import Data.Functor
import Control.Monad
import Text.Parsec hiding (Line, lookAhead, spaces)
import Text.Parsec.String
import qualified Text.Parsec as P

import Types
import OneLineParser

parseRange :: String -> Either String [Word16]
parseRange = parseOneLinePure rangeParser "command line"

rangeParser :: Parser [Word16]
rangeParser = concat <$> oneRangeParser `sepBy1` many1 (P.char ' ' <|> P.char ',')

oneRangeParser = do
    n <- read `fmap` many1 digit <?> "Number"
    skipMany (char ' ')
    choice
        [ do char '-'
             n' <- read `fmap` many1 digit <?> "Number"
             unless (n' > n) $ fail $ printf "%d is not larger than %d" n' n
             return [n..n']
        ,    return [n]
        ]
