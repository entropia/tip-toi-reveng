module OneLineParser (parseOneLine, parseOneLinePure) where

import System.Exit
import Text.Parsec hiding (Line, lookAhead, spaces)
import Text.Parsec.String
import Text.Parsec.Error
import qualified Text.Parsec as P

-- Parser utilities
-- | A nicer way to print an error message


lineParserErrorMessage :: String -> ParseError -> String
lineParserErrorMessage input err =
    "In " ++ sourceName pos ++ " column " ++ show (sourceColumn pos) ++ ":\n" ++
    input ++ "\n" ++
    replicate (sourceColumn pos - 1) ' ' ++ "↑" ++
    showErrorMessages "or" "unknown parse err" "expecting"
                      "unexpected" "end of input"
                      (errorMessages err)
  where pos = errorPos err

parseOneLine :: Parser a -> String -> String -> IO a
parseOneLine p name input =
    case parseOneLinePure p name input of
        Left e ->  putStrLn e >> exitFailure
        Right l -> return l

parseOneLinePure :: Parser a -> String -> String -> Either String a
parseOneLinePure p name input =
    case P.parse p name input of
        Left e ->  Left $ lineParserErrorMessage input e
        Right l -> return l
