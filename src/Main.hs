{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import LCDiagram.Options
import LCDiagram.Bytecode.Compiler
import LCDiagram.Bytecode.Execution (runLC)
import LCDiagram.Bytecode.Parser (encodeSymbolTable)
import LCDiagram.Repl
import System.Directory

main :: IO ()
main = do
  Options {command, files, outputPath} <- getArgs >>= compilerOpts

  case command of
    Repl -> lcRepl . mconcat =<< mapM compileFile files
    Run ->
      case files of
        [] -> lcRepl []
        _ -> print @Int =<< runLC . mconcat =<< mapM compileFile files
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
