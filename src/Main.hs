{-# LANGUAGE BlockArguments #-}

module Main where

import LCDiagram.Bytecode.Compiler
import LCDiagram.Bytecode.Execution (runLC)
import LCDiagram.Bytecode.Types
import LCDiagram.Parser
import Text.Megaparsec
import LCDiagram.Repl (repl)

main :: IO ()
main = do
  file <- getArgs >>= maybe (fail "Needs a File argument") pure . viaNonEmpty head
  case file of
    "--repl" -> repl
    _ -> do
      lcFile <- readFileBS file >>= either (fail . show) pure . decodeUtf8'

      (syms :: SymbolTable Int) <- case runParser lcParser "" lcFile of
        Left e -> fail $ errorBundlePretty e
        Right x -> return (lcCompiler x)

      runLC syms >>= \(x :: Int) -> print x
