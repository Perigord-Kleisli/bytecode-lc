{-# LANGUAGE RecordWildCards #-}

module LCDiagram.Repl (lcRepl) where

import Control.Exception (IOException)
import Control.Lens
import Control.Monad.Catch
import Data.Char (isSpace)
import Data.Map qualified as M
import System.Console.Haskeline
import System.Console.Haskeline.History (historyLines)
import System.Directory
import System.FilePath
import Text.Megaparsec hiding (try)

import LCDiagram.Bytecode.Compiler
import LCDiagram.Bytecode.Execution
import LCDiagram.Bytecode.Interpreter (VM, VMState (..))
import LCDiagram.Bytecode.Types
import LCDiagram.Parser

lcRepl :: (VM m a, m ~ StateT (VMState a) IO, a ~ Int) => SymbolTable a -> IO ()
lcRepl table = do
  historyFile <- liftIO $ fmap (</> "lc") <$> lookupEnv "XDG_STATE_HOME"
  let autoAddHistory = True
      fnCompletion = completeWord' Nothing isSpace \s -> do
        stackFrame <- gets (^. #stack . _head . #symbols)
        closure <- gets (^. #stack . _head . #instructions . #_Function . #captures)
        globals <- gets (^. #globals)
        let keys = map (toString . fst) $ M.toList (stackFrame <> closure <> globals)
        return
          [ Completion k k True
          | k <- ["trace", "import", "read"] <> keys
          , '.' `notElem` k
          , s `isPrefixOf` k
          ]
      complete =
        completeQuotedWord
          Nothing
          "\""
          ( \s -> case takeDirectory s of
              "." -> do
                importPaths <- maybe [] (splitOn (== ':')) <$> lookupEnv "LC_IMPORT_PATH"
                liftIO ((: importPaths) <$> getCurrentDirectory)
                  >>= foldMapM \targetDir -> do
                    contents <- liftIO (listDirectory targetDir)
                    areDirs <- liftIO $
                      forM contents \x ->
                        liftA2 (||) (doesDirectoryExist x) (doesDirectoryExist $ targetDir </> x)
                    return
                      [ let f' = (if isDir then f <> "/" else f)
                         in Completion f' f' (not isDir)
                      | (f, isDir) <- zip contents areDirs
                      , s `isPrefixOf` f
                      ]
              dir -> do
                importPaths <- maybe [] (splitOn (== ':')) <$> lookupEnv "LC_IMPORT_PATH"
                liftIO (map (</> dir) . (: importPaths) <$> getCurrentDirectory)
                  >>= foldMapM \targetDir -> do
                    ifM
                      (liftIO $ doesDirectoryExist targetDir)
                      ( do
                          contents <- liftIO (listDirectory targetDir)
                          areDirs <- liftIO $
                            forM contents \x ->
                              liftA2 (||) (doesDirectoryExist x) (doesDirectoryExist $ targetDir </> x)
                          return
                            [ let f' = (if isDir then f <> "/" else f)
                               in Completion (dir </> f') f' (not isDir)
                            | (f, isDir) <- zip contents areDirs
                            , takeFileName s `isPrefixOf` f
                            ]
                      )
                      (return [])
          )
          fnCompletion

  evalStateT
    (runInputT Settings {..} loop)
    VMState
      { succesor = (+ 1)
      , stack = []
      , input = 0
      , globals = table
      }
  where
    handleCommands [] = do
      getHistory
        >>= ( \case
                Just ":" -> loop
                Just (toString -> (':' : cmd)) -> handleCommands (words $ fromString cmd)
                _ -> loop
            )
          . find (":" `isPrefixOf`)
          . historyLines
    handleCommands ["q"] = pass
    handleCommands ["quit"] = pass
    handleCommands x = do
      putTextLn $ "unknown command '" <> unwords x <> "'"
      loop
    loop = do
      getInputLine "λ> " >>= \case
        Just (':' : minput) -> handleCommands (words $ fromString minput)
        Just "" -> loop
        Just minput -> do
          catch
            do
              case runParser ((lcImport <|> lcDec) <* eof) "<REPL DECLARATION>" (fromString minput) of
                Right (ImportDec path) -> do
                  syms <- liftIO $ compileFile path
                  lift $ #globals <>= syms
                  loop
                Right dec -> do
                  lift $ #globals <>= lcCompiler [dec]
                  loop
                Left _ -> do
                  case runParser (lcExpr <* eof) "<REPL EXPR>" (" " <> fromString minput) of
                    Right expr -> do
                      table' <- lift $ gets (^. #globals)
                      v <- liftIO $ runLC (lcCompiler [Def "main" expr] <> table')
                      print v
                      loop
                    Left e -> fail $ errorBundlePretty e
            ( \(e :: IOException) -> do
                print e
                loop
            )
        Nothing -> pass

splitOn :: (Char -> Bool) -> String -> [String]
splitOn f s = case dropWhile f s of
  "" -> []
  s' -> w : splitOn f s''
    where
      (w, s'') =
        break f s'
