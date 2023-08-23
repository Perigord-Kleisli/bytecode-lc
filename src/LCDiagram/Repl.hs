{-# LANGUAGE NamedFieldPuns #-}

module LCDiagram.Repl (lcRepl) where

import Control.Exception (IOException)
import Control.Lens
import Control.Monad.Catch
import System.Console.Haskeline
import System.FilePath
import Text.Megaparsec hiding (try)

import LCDiagram.Bytecode.Compiler
import LCDiagram.Bytecode.Execution
import LCDiagram.Bytecode.Interpreter (VM, VMState (..))
import LCDiagram.Bytecode.Types
import LCDiagram.Parser
import LCDiagram.Repl.Commands
import LCDiagram.Repl.Completion (lcCompletion)

lcRepl :: (VM m a, m ~ StateT (VMState a) IO, a ~ Int) => SymbolTable a -> IO ()
lcRepl table = do
  historyFile <- liftIO $ fmap (</> "lc") <$> lookupEnv "XDG_STATE_HOME"
  evalStateT
    ( runInputT
        Settings
          { complete = lcCompletion
          , historyFile
          , autoAddHistory = True
          }
        loop
    )
    VMState
      { succesor = (+ 1)
      , stack = []
      , input = 0
      , globals = table
      , mainFileDir = "."
      }
  where
    loop = do
      getInputLine "λ> " >>= \case
        Just (':' : minput) -> case runParser cmdParser "<REPL>" minput of
          Right cmd -> do
            lcCommand loop cmd
          Left e -> do
            putStrLn $ errorBundlePretty e
            loop
        Just minput -> do
          catch
            do
              case runParser ((lcImport <|> lcDec) <* eof) "<REPL DECLARATION>" (fromString minput) of
                Right (ImportDec path) -> do
                  fileDir <- lift $ gets mainFileDir
                  syms <- liftIO $ compileFile (fileDir </> path)
                  lift $ #globals <>= syms
                  loop
                Right dec -> do
                  lift $ #globals <>= lcCompiler [dec]
                  loop
                Left _ -> do
                  case runParser (lcExpr <* eof) "<REPL EXPR>" (" " <> fromString minput) of
                    Right expr -> do
                      table' <- lift $ gets (^. #globals)
                      mainFileLoc <- lift $ gets mainFileDir
                      v <- liftIO $ runLC mainFileLoc (lcCompiler [Def "main" expr] <> table')
                      print v
                      loop
                    Left e -> fail $ errorBundlePretty e
            ( \(e :: IOException) -> do
                print e
                loop
            )
        Nothing -> pass
