{-# LANGUAGE OverloadedLists #-}

module LCDiagram.Bytecode.Compiler (lcCompiler, compileFileWithHash, readFromImportPath, compileFile) where

import Control.Arrow (ArrowChoice (left))
import Control.Exception (throw)
import Control.Lens
import Data.Digest.Pure.MD5 (MD5Digest, md5)
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Sequence qualified as Se
import Data.Set qualified as S
import LCDiagram.Bytecode.Parser
import LCDiagram.Bytecode.Types
import LCDiagram.Parser
import LCDiagram.Types (LCError (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.Megaparsec (runParser)
import Prelude hiding (toStrict)

data CompilerState a = CompilerState
  { symbols :: SymbolTable a
  , focusedName :: Text
  }
  deriving stock (Show, Generic)

type Compiler a = State (CompilerState a)

lcCompiler :: [LCDec] -> SymbolTable a
lcCompiler src = execState (mapM_ lcDecCompiler src) (CompilerState [] "") ^. #symbols
  where
    lcDecCompiler :: LCDec -> Compiler a ()
    lcDecCompiler (ImportDec path) = do
      #symbols %= M.insert "imports" (Function $ FnVals {code = [Import path], captures = []})
    lcDecCompiler (Def name expr) = do
      #focusedName .= name
      expr' <- lcExprCompiler expr
      #symbols %= M.insert name (Function $ FnVals (expr' Se.|> Call) [])

    uniqueName name = do
      syms <- gets (^. #symbols)
      if M.member name syms
        then uniqueName (name <> "_")
        else return name

    lcExprCompiler :: LCExpr -> Compiler a (Seq Instruction)
    lcExprCompiler (Abs arg body) = do
      oldName <- gets focusedName
      lambdaName <- uniqueName (oldName <> "." <> arg)
      body' <- lcExprCompiler body
      #symbols %= M.insert lambdaName (Function $ FnVals ([Store arg] <> body') [])
      let caps = map MakeClosure $ capturedVals [arg] body
      return $ [Load lambdaName] <> fromList caps
    lcExprCompiler (App f arg) = do
      f' <- lcExprCompiler f
      arg' <- lcExprCompiler arg
      return $ arg' <> f' <> [Call]
    lcExprCompiler (Var name) = return [Load name]

    capturedVals :: Set Text -> LCExpr -> [Text]
    capturedVals passedVals (Var x)
      | S.member x passedVals = []
      | otherwise = [x]
    capturedVals passedVals (App f x) =
      capturedVals passedVals f <> capturedVals passedVals x
    capturedVals passedVals (Abs arg body) =
      capturedVals (S.insert arg passedVals) body

splitOn :: (Char -> Bool) -> String -> [String]
splitOn f s = case dropWhile f s of
  "" -> []
  s' -> w : splitOn f s''
    where
      (w, s'') =
        break f s'

readFromImportPathWithHash :: FilePath -> IO (MD5Digest, ByteString)
readFromImportPathWithHash path = do
  paths <- maybe [] (splitOn (== ':')) <$> lookupEnv "LC_IMPORT_PATH"
  maybe (fail $ "file '" <> path <> "' does not exist\n" <> "searched: " <> show paths) pure . viaNonEmpty head =<< mapMaybeM getExisting (path : paths)
  where
    getExisting path' =
      (\x -> (md5 (toLazy x), x))
        <<$>> ( do
                  ifM
                    (doesFileExist path')
                    (Just <$> readFileBS path')
                    $ ifM
                      (doesFileExist $ path' <> ".lc")
                      (Just <$> readFileBS (path' <> ".lc"))
                    $ ifM
                      (doesFileExist $ path' <> ".lc.o")
                      (Just <$> readFileBS (path' <> ".lc.o"))
                    $ ifM
                      (doesFileExist (path' </> path))
                      (Just <$> readFileBS (path' </> path))
                    $ ifM
                      (doesFileExist (path' </> path <> ".lc"))
                      (Just <$> readFileBS (path' </> path <> ".lc"))
                    $ ifM
                      (doesFileExist (path' </> path <> "lc.o"))
                      (Just <$> readFileBS (path </> path <> "lc.o"))
                      (pure Nothing)
              )

readFromImportPath :: FilePath -> IO ByteString
readFromImportPath = fmap snd . readFromImportPathWithHash

compileBS :: Maybe FilePath -> ByteString -> Either LCError (SymbolTable a)
compileBS (fromMaybe "" -> file) conts =
  if hasLcoHeader conts
    then decodeSymbolTable conts file
    else
      bimap OtherError lcCompiler
        . runParser lcParser file
        =<< left OtherError (decodeUtf8' conts)

compileFileWithHash :: FilePath -> IO (MD5Digest, SymbolTable a)
compileFileWithHash file = do
  (hash, conts) <- readFromImportPathWithHash file
  either throw (pure . (hash,)) $ compileBS (Just file) conts

compileFile :: FilePath -> IO (SymbolTable a)
compileFile = fmap snd . compileFileWithHash
