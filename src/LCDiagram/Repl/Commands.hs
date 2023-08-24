{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoOverloadedLists #-}

module LCDiagram.Repl.Commands (cmdParser, lcCommand) where

import Control.Lens
import Data.Map qualified as M
import LCDiagram.Bytecode.Compiler (compileFileWithHash)
import LCDiagram.Bytecode.Interpreter
import LCDiagram.Repl.Types (Repl, ReplState (..))
import PyF
import System.Console.Haskeline
import System.Console.Haskeline.History (historyLines)
import System.Directory
import System.FilePath
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (many, some)

data Command = Command
  { cmd :: String
  , args :: [String]
  }
  deriving stock (Show)

cmdParser :: Parsec Void String Command
cmdParser = do
  _ <- optional (char ':')
  cmd <- manyTill anySingle (void (char ' ') <|> eof)
  args <- manyTill (someTill (spaceEsc <|> L.charLiteral) (void (some (char ' ')) <|> eof)) eof
  return Command {..}
  where
    spaceEsc = char '\\' *> char ' '

lcCommand :: Repl a () -> Command -> Repl a ()
lcCommand loop = \case
  (Command "q" _) -> pass
  (Command "quit" _) -> pass
  (Command "r" _) -> do
    reloadCmd
    loop
  (Command "reload" _) -> do
    reloadCmd
    loop
  (Command "pwd" _) -> do
    lift $ gets (mainFileDir . vm) >>= putStrLn
    loop
  (Command "cd" []) -> do
    curDir <- liftIO getCurrentDirectory
    lift $ #vm . #mainFileDir .= curDir
    loop
  (Command "cd" (path : _)) -> do
    ifM
      (liftIO $ doesDirectoryExist (toString path))
      do
        lift $ #vm . #mainFileDir %= changeDir path
        loop
      do
        putStrLn $ "path '" <> path <> "' does not exist"
        loop
  (Command "" args) -> do
    hist <- historyLines <$> getHistory
    let lastCmd = viaNonEmpty head $ filter ((\x -> x /= "q" && x /= "") . cmd) $ snd $ partitionWith (runParser cmdParser "<LAST CMD>") hist
    case lastCmd of
      Just (Command cmd args2) -> do
        outputStrLn $ "> :" <> cmd <> " " <> toString (unwords $ map toText (args <> args2))
        lcCommand loop (Command cmd (args2 <> args))
      Nothing -> do
        outputStrLn "No last command"
        loop
  (Command cmdname _) -> do
    putStrLn [fmt|No command '{cmdname}'|]
    loop

changeDir :: FilePath -> FilePath -> FilePath
changeDir toPath fromPath =
  if isAbsolute toPath
    then toPath
    else normalise $ expandPath $ splitDirectories (fromPath </> toPath)
  where
    expandPath (_ : ".." : xs) = expandPath xs
    expandPath (x : "." : xs) = x </> expandPath xs
    expandPath (x : xs) = x </> expandPath xs
    expandPath [] = ""

reloadCmd :: Repl a ()
reloadCmd = void do
  lift (gets loadedFiles) >>= M.traverseWithKey \path hash -> do
    (hash', syms) <- liftIO $ compileFileWithHash path
    unless (hash == hash') do
      putStrLn $ "Reloading file: '" <> path <> "'"
      lift do
        #vm . #globals %= M.union syms
        #loadedFiles . ix path .= hash'
