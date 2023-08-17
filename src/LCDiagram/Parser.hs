{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoOverloadedLists #-}

module LCDiagram.Parser (LCExpr (..), LCDec (..), lcParser, lcExpr, lcDec, lcImport, Parser) where

import Data.Char
import PyF (fmt)
import Text.Megaparsec as M
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Parser.Combinators qualified as C

data LCExpr
  = App LCExpr LCExpr
  | Var Text
  | Abs Text LCExpr
  deriving stock (Show, Eq)

data LCDec
  = Def Text LCExpr
  | ImportDec FilePath
  deriving stock (Show, Eq)

type Parser = Parsec Void Text

space :: Parser ()
space = L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented space

indented :: Parser a -> Parser a
indented p = L.indentGuard space GT pos1 *> p

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

parens :: forall {a}. Parser a -> Parser a
parens = between (symbol "(") (symbol ")") . lexeme

reserved :: [Tokens Text]
reserved = ["import"]

identifier :: Parser Text
identifier =
  takeWhile1P (Just "a valid Var character") validVarChar >>= \s ->
    if s `elem` reserved
      then fail [fmt|Identifier cannot be reserved keyword '{s}'|]
      else return s
  where
    validVarChar c
      | isSpace c
          || isDigit c
          || isControl c
          || c `elem` ("().\\λ" :: String) =
          False
      | otherwise = True

defIdentifier :: Parser Text
defIdentifier =
  takeWhile1P (Just "a valid Var character") validVarChar >>= \s ->
    if s `elem` reserved
      then fail [fmt|Identifier cannot be reserved keyword '{s}'|]
      else return s
  where
    validVarChar c
      | isSpace c
          || isDigit c
          || isControl c
          || c `elem` ("().\\λ=" :: String) =
          False
      | otherwise = True

var :: Parser LCExpr
var = Var <$> identifier

fromNumber :: Parser LCExpr
fromNumber = Abs "f" . Abs "x" . intToChurch <$> L.decimal
  where
    intToChurch :: Int -> LCExpr
    intToChurch 0 = Var "x"
    intToChurch n = App (Var "f") (intToChurch (n - 1))

abstraction :: Parser LCExpr
abstraction = do
  _ <- char 'λ' <|> char '\\'
  args <- M.some (lexeme identifier) <?> "function arguments"
  _ <- symbol "." <|> symbol "->"
  expr <- lcExpr <?> "valid function definition"
  return $ foldr Abs expr args

term :: Parser LCExpr
term = indented $ choice [parens lcExpr, fromNumber, var, abstraction]

application :: Parser LCExpr
application = C.chainl1 (lexeme term) (pure App)

lcExpr :: Parser LCExpr
lcExpr = indented $ choice [application, term]

lcDec :: Parser LCDec
lcDec = nonIndented $ Def <$> (lexeme defIdentifier <* symbol "=") <*> lcExpr

lcImport :: Parser LCDec
lcImport =
  nonIndented
    . fmap ImportDec
    $ lexeme
    $ symbol "import" *> ((char '"' >> manyTill L.charLiteral (char '"')) <?> "string literal")

lcParser :: Parser [LCDec]
lcParser =
  M.some
    ( choice
        [ lcImport <?> "Import Statement"
        , lcDec <?> "Variable Definition"
        ]
    )
    <* eof
