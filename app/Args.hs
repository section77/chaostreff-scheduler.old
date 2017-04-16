module Args
    (
     parseArgs
    , AppArgs (..)
    ) where

import           Protolude            hiding (try, (<|>))
import           Text.Parsec
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as P
import           Types                (Month, Year)

data AppArgs = ShowHelp
             | ShowVersion
             | ScheduleNextMonths {
                cfgFile        :: FilePath
              , numberOfMonths :: Int
               }
             | ScheduleMonth {
                 cfgFile :: FilePath
               , year    :: Year
               , month   :: Month
               } deriving Show


parseArgs :: [Char] -> Either ParseError AppArgs
parseArgs = parse p "parse args"
    where p = try parseHelp
              <|> try parseVersion
              <|> try parseScheduleMonth
              <|> try parseScheduleNextMonths


parseHelp :: Parser AppArgs
parseHelp = do
  _ <- string "-h" <* eof
  return ShowHelp


parseVersion :: Parser AppArgs
parseVersion = do
  _ <- string "-v" <* eof
  return ShowVersion


parseScheduleNextMonths :: Parser AppArgs
parseScheduleNextMonths = do
  cfg <- word
  n <- fromIntegral <$> integer <|> (pure 1) <* eof
  return $ ScheduleNextMonths cfg n


parseScheduleMonth :: Parser AppArgs
parseScheduleMonth = do
  cfg <- word
  y <- integer
  m <- fromIntegral <$> integer <* eof
  return $ ScheduleMonth cfg y m


-- parsec
lexer :: P.GenTokenParser [Char] u Identity
lexer = P.makeTokenParser emptyDef

integer :: ParsecT [Char] u Identity Integer
integer = P.integer lexer

word :: ParsecT [Char] u Identity [Char]
word = many1 $ noneOf " "
