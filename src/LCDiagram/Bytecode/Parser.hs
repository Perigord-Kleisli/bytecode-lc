module LCDiagram.Bytecode.Parser where

import Text.Megaparsec

type Parser a = Parsec Void ByteString
