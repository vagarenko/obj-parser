module ObjParser.Utils (
    -- * Utility parsers
    trimLineR,
    float,
    skipHSpace,
    vector3f,
    decimal1,
    fileName,
    traceParser,
    traceParserName
) where

import Control.Applicative   ((<|>))
import Data.Attoparsec.Text  (Parser, endOfLine, double, (<?>), skipWhile, decimal, isHorizontalSpace, char, takeWhile1, match)
import Data.Char             (ord)
import Debug.Trace           (traceShowM)
import qualified Data.Text as T (unpack)
import Linear

-- | Skip horizontal spaces until end of the line.
trimLineR :: Parser ()
trimLineR = skipHSpace *> endOfLine

-- | Parse single float number.
float :: Parser Float
float = realToFrac <$> double
    <?> "Float"

-- | Skip zero or more horizontal spaces.
skipHSpace :: Parser ()
skipHSpace = skipWhile isHorizontalSpace
    <?> "SkipHSpace"

-- | Parse three float number separated by any number of horizontal spaces.
vector3f :: Parser (V3 Float)
vector3f = V3 <$> float <*> (skipHSpace *> float) <*> (skipHSpace *> float)

-- | Unsigned decimal number greater than 0
decimal1 :: Integral a => Parser a
decimal1 = do
    d <- decimal
    if d == 0
        then fail "Expected non-zero decimal"
        else return d
    <?> "Decimal1"

-- | Parse UTF filename. Can contain spaces if quoted.
fileName :: Parser FilePath
fileName = 
    T.unpack <$> ((char '"' *> takeWhile1 (\c -> allowedChar c || ' ' == c) <* char '"') <|> takeWhile1 allowedChar)
    <?> "FileName"
    where
        allowedChar c = ord c > 32 && c `notElem` "?%*:|\"<>"

traceParser :: Show a => Parser a -> Parser a
traceParser p = do
    (t, p') <- match p
    traceShowM (t, p')
    return p'

traceParserName :: Show a => String -> Parser a -> Parser a
traceParserName n p = do
    (t, p') <- match p
    traceShowM (n, t, p')
    return p'