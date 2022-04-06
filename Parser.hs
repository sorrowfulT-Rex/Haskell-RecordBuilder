module Parser where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data
import           Data.Bifunctor
import           Data.Functor
import qualified Data.Map as M
import           Data.Maybe
import           Data.Void
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

type ParserT m = P.ParsecT Void String m

parser :: Monad m => ParserT (StateT (Int, Int) m) [Data]
parser = catMaybes <$>
  P.sepEndBy dataParser (spaceConsumer >> P.some (void (P.oneOf "\r\n")))

spaceConsumer :: Monad m => ParserT m ()
spaceConsumer = void $ P.many (P.noneOf "\r\n")

dataParser :: Monad m => ParserT (StateT (Int, Int) m) (Maybe Data)
dataParser = do
  header <- string "data" P.<|> string "newtype" P.<|> string ""
  case header of
    "" -> spaceConsumer $> Nothing
    _  -> do
      lift $ put (0, 0)
      name   <- unwords <$> P.sepEndBy1 (P.some nameToken) (P.some P.space1)
      void $ char '='
      cons   <- fmap M.fromList . flip P.sepBy1 (char '|') . lexeme $ do
        consName   <- lexeme $ P.some nameToken
        consFields <- lexeme $ M.fromList <$> P.choice [named, nameless]
        return (consName, consFields)
      maxPre <- lift $ gets snd
      return . Just $ Data (maxPre + 1) name cons
  where
    nameToken = P.choice [P.alphaNumChar, P.oneOf "_'"]
    lexeme    = L.lexeme P.space
    char      = lexeme . P.char
    string    = lexeme . P.string
    named     = P.between (char '{') (char '}')
              . lexeme $ flip P.sepBy1 (char ',')
              . lexeme $ do
      recordName <- L.lexeme P.space $ P.some nameToken
      lift (modify (second $ max (length (takeWhile (== '_') recordName))))
      void $ string "::"
      recordType <- unwords <$> P.sepEndBy1 (P.some typeToken) (P.some P.space1)
      return (recordName, recordType)
    nameless  = P.many (P.noneOf "|}") $> []
    typeToken = P.choice [ nameToken, P.oneOf "[]!'"
                         , P.char '(' <* lift (modify $ first succ)
                         , P.char ')' <* lift (modify $ first pred)
                         , do parenDepth <- lift $ gets fst
                              if parenDepth > 0 then P.char ',' else P.empty ]
