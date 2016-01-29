module Args
    (
     parseArgs
    , AppArgs (..)
    ) where

import           Text.Parsec
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as P
import           Types                (Month, Year)

data AppArgs = ShowHelp
             | ShowVersion
             | ScheduleNextMonths {
                cfgFile :: FilePath
              , n       :: Int
               }
             | ScheduleMonth {
                 cfgFile :: FilePath
               , year    :: Year
               , month   :: Month
               } deriving Show


parseArgs :: String -> Either ParseError AppArgs
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
  cfgFile <- word
  n <- fromIntegral <$> integer <|> (pure 1) <* eof
  return $ ScheduleNextMonths cfgFile n


parseScheduleMonth :: Parser AppArgs
parseScheduleMonth = do
  cfgFile <- word
  year <- integer
  month <- fromIntegral <$> integer <* eof
  return $ ScheduleMonth cfgFile year month


-- parsec
lexer = P.makeTokenParser emptyDef
symbol = P.symbol lexer
integer = P.integer lexer
word = many1 $ noneOf " "
