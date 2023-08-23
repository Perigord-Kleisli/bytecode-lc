module LCDiagram.Repl.Completion (lcCompletion) where

import Data.Map qualified as M
import System.Console.Haskeline

import Control.Lens
import Data.Char
import LCDiagram.Bytecode.Interpreter
import System.Directory
import System.FilePath

lcCompletion :: CompletionFunc (StateT (VMState a) IO)
lcCompletion (l, r)
  | "import" `isPrefixOf` reverse l = completeQuotedWord (Just '\\') ['"'] dirCompletion noCompletion (l, r)
  | ":" `isPrefixOf` reverse l = completeWord' Nothing isSpace cmdCompletion (l, r)
  | otherwise = fnCompletion (l, r)

cmdCompletion :: String -> StateT (VMState a) IO [Completion]
cmdCompletion (':' : s) =
  do pure
    [ Completion (':' : cmd) cmd True
    | cmd <- ["pwd", "q", "quit", "cd", "r", "reload"]
    , s `isPrefixOf` cmd
    ]
cmdCompletion s = snd <$> completeWord' Nothing isSpace dirCompletion (reverse s, [])

fnCompletion :: CompletionFunc (StateT (VMState a) IO)
fnCompletion = completeWord' Nothing isSpace \s -> do
  stackFrame <- gets (^. #stack . _head . #symbols)
  closure <- gets (^. #stack . _head . #instructions . #_Function . #captures)
  globals' <- gets (^. #globals)
  let keys = map (toString . fst) $ M.toList (stackFrame <> closure <> globals')
  return $
      [ Completion (if k == "import" then "import \"" else k) k (k /= "import")
      | k <- [":", "trace", "read", "import"] <> keys
      , '.' `notElem` k
      , s `isPrefixOf` k
      , k /= "main"
      ]

dirCompletion :: FilePath -> StateT (VMState a) IO [Completion]
dirCompletion s = case takeDirectory s of
  "." -> do
    importPaths <- maybe [] splitSearchPath <$> lookupEnv "LC_IMPORT_PATH"
    rootDir <- gets mainFileDir
    flip foldMapM (rootDir : importPaths) \targetDir -> do
      contents <-
        liftIO $
          listDirectory targetDir >>= mapM \x -> do
            ifM
              (liftA2 (||) (doesDirectoryExist x) (doesDirectoryExist $ targetDir </> x))
              (pure $ addTrailingPathSeparator x)
              (pure x)
      return
        [ Completion f f (not (hasTrailingPathSeparator f))
        | f <- contents
        , s `isPrefixOf` f
        , hasTrailingPathSeparator f
            || isExtensionOf "lc" f
            || isExtensionOf "lc.o" f
        ]
  dir -> do
    importPaths <- maybe [] splitSearchPath <$> lookupEnv "LC_IMPORT_PATH"
    rootDir <- gets mainFileDir
    flip foldMapM (map (</> dir) (rootDir : importPaths)) \targetDir -> do
      ifM
        (liftIO $ doesDirectoryExist targetDir)
        do
          contents <-
            liftIO $
              listDirectory targetDir >>= mapM \x -> do
                ifM
                  (liftA2 (||) (doesDirectoryExist x) (doesDirectoryExist $ targetDir </> x))
                  (pure $ addTrailingPathSeparator x)
                  (pure x)
          return
            [ Completion (dir </> f) f (not (hasTrailingPathSeparator f))
            | f <- contents
            , takeFileName s `isPrefixOf` f
            , hasTrailingPathSeparator f
                || isExtensionOf "lc" f
                || isExtensionOf "lc.o" f
            ]
        (return [])
