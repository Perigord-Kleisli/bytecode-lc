{-# LANGUAGE BlockArguments #-}
module LCDiagram.Repl where

import System.Console.Haskeline
import LCDiagram.Bytecode.Types
import Text.Megaparsec
import LCDiagram.Parser
import LCDiagram.Bytecode.Compiler
import LCDiagram.Bytecode.Execution

repl :: IO ()
repl = runInputT defaultSettings loop
  where 
  loop = do 
    Just minput <- getInputLine "λ> "

    (syms :: SymbolTable Int) <- case runParser lcExpr "" (" " <> fromString minput) of
        Left e -> fail $ errorBundlePretty e
        Right x -> return (lcCompiler [Def "main" x])
    liftIO $ runLC syms >>= \(x :: Int) -> print x
    loop
