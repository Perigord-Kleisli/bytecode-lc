{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoOverloadedLists #-}

module LCDiagram.Repl.Commands (cmdParser, lcCommand) where

import Control.Lens
import LCDiagram.Bytecode.Interpreter
import PyF
import System.Console.Haskeline
import System.Console.Haskeline.History (historyLines, readHistory)
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

lcCommand :: InputT (StateT (VMState a) IO) () -> Command -> InputT (StateT (VMState a) IO) ()
lcCommand loop = \case
  (Command "q" _) -> pass
  (Command "quit" _) -> pass
  (Command "r" _) -> undefined "TODO: Module Reloading"
  (Command "reload" _) -> undefined "TODO: Module Reloading"
  (Command "pwd" _) -> do
    lift $ gets mainFileDir >>= putStrLn
    loop
  (Command "cd" []) -> do
    lift $ #mainFileDir .= "."
    loop
  (Command "cd" (path : _)) -> do
    ifM
      (liftIO $ doesDirectoryExist (toString path))
      do
        lift $ #mainFileDir %= (</> toString path)
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
