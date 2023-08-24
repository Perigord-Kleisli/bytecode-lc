{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.Map qualified as M
import LCDiagram.Bytecode.Compiler
import LCDiagram.Bytecode.Execution (runLC)
import LCDiagram.Bytecode.Parser (encodeSymbolTable)
import LCDiagram.Options
import LCDiagram.Repl
import System.Directory
import System.FilePath

findM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m (Maybe a)
findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)

getMainFile :: [FilePath] -> IO (Maybe FilePath)
getMainFile = findM (fmap (M.member "main") . compileFile)

main :: IO ()
main = do
  Options {command, files, outputPath} <- getArgs >>= compilerOpts

  case command of
    Repl -> do
      getMainFile files >>= \case
        Just file' -> lcRepl (takeDirectory file') . mconcat =<< mapM compileFile files
        Nothing -> do
          curDir <- getCurrentDirectory
          lcRepl curDir . mconcat =<< mapM compileFile files
    Run ->
      case files of
        [] -> getCurrentDirectory >>= flip lcRepl []
        _ -> do
          mainFile <-
            getMainFile files >>= \case
              Just x -> pure x
              Nothing -> fail "No main function located in any of the chosen files"
          syms <- mconcat <$> mapM compileFile files
          print @Int =<< runLC (takeDirectory mainFile) syms
    Link -> do
      case files of
        (_ : _ : _) -> do
          outPath' <- case outputPath of
            Just x -> pure x
            Nothing -> fail "--output must be specified when linking"
          writeFileBS outPath' . encodeSymbolTable . mconcat =<< mapM compileFile files
        _ -> fail "--link requires 2 or more file arguments"
    Compile -> do
      case files of
        [] -> fail "--compile requires file arguments"
        [file] -> compileFile file >>= writeFileBS (fromMaybe (file <> ".o") outputPath) . encodeSymbolTable
        _ -> do
          unlessM (maybe (pure True) doesDirectoryExist outputPath) do
            fail $ "Directory '" <> show (fromMaybe "" outputPath) <> "' does not exist"
          forM_ files \file -> do
            compileFile file >>= writeFileBS (fromMaybe (file <> ".o") outputPath) . encodeSymbolTable
