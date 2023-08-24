{-# LANGUAGE NamedFieldPuns #-}

module LCDiagram.Repl (lcRepl) where

import Control.Exception (IOException)
import Control.Lens
import Control.Monad.Catch
import System.Console.Haskeline
import System.FilePath
import Text.Megaparsec hiding (try)

import Data.Map qualified as M
import LCDiagram.Bytecode.Compiler
import LCDiagram.Bytecode.Execution
import LCDiagram.Bytecode.Interpreter (VMState (..))
import LCDiagram.Bytecode.Types
import LCDiagram.Parser
import LCDiagram.Repl.Commands
import LCDiagram.Repl.Completion (lcCompletion)
import LCDiagram.Repl.Types (ReplState (..))

lcRepl :: FilePath -> SymbolTable Int -> IO ()
lcRepl path table = do
  historyFile <- liftIO $ fmap (</> "lc") <$> lookupEnv "XDG_STATE_HOME"
  stdLib <- compileFile "std"
  evalStateT
    ( runInputT
        Settings
          { complete = lcCompletion
          , historyFile
          , autoAddHistory = True
          }
        loop
    )
    ReplState
      { vm =
          VMState
            { succesor = (+ 1)
            , stack = []
            , input = 0
            , globals = table <> stdLib
            , mainFileDir = path
            }
      , loadedFiles = []
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
                Right (ImportDec file) -> do
                  fileDir <- lift $ gets (mainFileDir . vm)
                  (hash, syms) <- liftIO $ compileFileWithHash (fileDir </> file)
                  lift do
                    #vm . #globals %= M.union syms
                    #loadedFiles %= M.insert file hash
                  loop
                Right dec -> do
                  lift $ #vm . #globals <>= lcCompiler [dec]
                  loop
                Left _ -> do
                  case runParser (lcExpr <* eof) "<REPL EXPR>" (" " <> fromString minput) of
                    Right expr -> do
                      table' <- lift $ gets (^. #vm . #globals)
                      mainFileLoc <-
                        lift $
                          gets
                            (mainFileDir . vm)
                      handle (\Interrupt -> outputStrLn "Interrupted.") $
                        withInterrupt do
                          v <- liftIO $ runLC mainFileLoc (lcCompiler [Def "main" expr] <> table')
                          print v
                      loop
                    Left e -> fail $ errorBundlePretty e
            ( \(e :: IOException) -> do
                print e
                loop
            )
        Nothing -> pass
