{-# LANGUAGE BlockArguments #-}

module LCDiagram.Repl (repl) where

import Control.Exception (IOException)
import Control.Monad.Catch
import LCDiagram.Bytecode.Compiler
import LCDiagram.Bytecode.Execution
import LCDiagram.Bytecode.Types
import LCDiagram.Parser
import System.Console.Haskeline
import Text.Megaparsec
import Shower (shower)

repl :: IO ()
repl = runInputT defaultSettings (loop [])
  where
    loop :: SymbolTable Int -> InputT IO ()
    loop syms = do
      minput <- getInputLine "λ> "
      case minput of
        Just ":quit" -> pass
        Just ":q" -> pass
        Just "" -> loop syms
        Just minput' -> do
          catch
            do
              case runParser (lcDec <* eof) "" (fromString minput') of
                Right dec -> do
                  loop (syms <> lcCompiler [dec])
                Left _ -> pass
              case runParser (lcExpr <* eof) "" (" " <> fromString minput') of
                Right expr -> do 
                  (v :: Int) <- liftIO $ runLC (lcCompiler [Def "main" expr] <> syms)
                  print v
                  loop syms
                Left e -> fail $ errorBundlePretty e
            ( \(e :: IOException) -> do
                print e
                loop syms
            )
        _ -> loop syms

    exprOrDec :: Parser (Either LCExpr LCDec)
    exprOrDec = (Left <$> lcExpr) <|> (Right <$> lcDec)
