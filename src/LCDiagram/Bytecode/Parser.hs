module LCDiagram.Bytecode.Parser (encodeSymbolTable, decodeSymbolTable) where

import Data.ByteString (pack)
import Data.Map qualified as Map
import LCDiagram.Bytecode.Types
import Text.Megaparsec
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Binary

type Parser a = Parsec Void ByteString a

instruction :: Parser Instruction
instruction =
  word8 >>= \case
    1 -> pure Call
    2 -> pure Trace
    3 -> pure Read
    4 -> do
      name <- manyTill word8 (char 41)
      pure (MakeClosure (decodeUtf8 $ pack name))
    5 -> do
      name <- manyTill word8 (char 41)
      pure (Store (decodeUtf8 $ pack name))
    6 -> do
      name <- manyTill word8 (char 41)
      pure (Load (decodeUtf8 $ pack name))
    7 -> do
      name <- manyTill word8 (char 41)
      pure (Import (decodeUtf8 $ pack name))
    _ -> error "Invalid instruction"

binding :: Parser (Text, Function a)
binding = do
  name <- decodeUtf8 . pack <$> manyTill word8 (char 41)
  instructions' <- fromList <$> manyTill instruction (char 10)
  pure (name, Function $ FnVals instructions' [])

lcoHeader :: ByteString
lcoHeader = pack [1, 9, 3, 0]

symboltable :: Parser (SymbolTable a)
symboltable = do
  _ <- string lcoHeader <?> "LCO Header"
  Map.fromList <$> M.many binding

encodeSymbolTable :: SymbolTable a -> ByteString
encodeSymbolTable syms =
  lcoHeader <> foldr ((\x y -> x <> "\n" <> y) . encodeBinding) "" (Map.toList syms)
  where
    encodeInstruction :: Instruction -> ByteString
    encodeInstruction Call = one 1
    encodeInstruction Trace = one 2
    encodeInstruction Read = one 3
    encodeInstruction (MakeClosure name) = one 4 <> encodeUtf8 name <> ")"
    encodeInstruction (Store name) = one 5 <> encodeUtf8 name <> ")"
    encodeInstruction (Load name) = one 6 <> encodeUtf8 name <> ")"
    encodeInstruction (Import name) = one 7 <> encodeUtf8 name <> ")"

    encodeBinding :: (Text, Function a) -> ByteString
    encodeBinding (name, Function (FnVals code' [])) = encodeUtf8 name <> ")" <> foldr (<>) "" (fmap encodeInstruction code')
    encodeBinding _ = error "Can only encode lambda functions"

shebang :: Parser ()
shebang = void $ optional (string "#!" *> manyTill anySingle eol)

decodeSymbolTable :: FilePath -> IO (SymbolTable a)
decodeSymbolTable file = do
  conts <- readFileBS file
  case runParser (shebang *> symboltable <* eof) file conts of
    Left e -> fail $ errorBundlePretty e
    Right x -> pure x
