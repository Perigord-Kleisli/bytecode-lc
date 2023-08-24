module LCDiagram.Options (Command (..), Options (..), compilerOpts) where

import PyF
import System.Console.GetOpt

data Command
  = Run
  | Compile
  | Repl
  | Link
  deriving stock (Show)

data Options = Options
  { command :: Command
  , files :: [FilePath]
  , outputPath :: Maybe FilePath
  }
  deriving stock (Show)

defaultOptions :: Options
defaultOptions = Options Run [] Nothing

options :: [OptDescr (Options -> Maybe Options)]
options =
  [ Option
      ['c']
      ["compile"]
      ( NoArg \case
          o@Options {command = Run} -> Just o {command = Compile}
          _ -> Nothing
      )
      "compile files"
  , Option
      []
      ["repl"]
      ( NoArg \case
          o@Options {command = Run} -> Just o {command = Repl}
          _ -> Nothing
      )
      "open Repl"
  , Option
      ['l']
      ["link"]
      ( NoArg \case
          o@Options {command = Run} -> Just o {command = Link}
          _ -> Nothing
      )
      "link multiple *.lco files"
  , Option
      ['o']
      ["output"]
      ( ReqArg
          ( \d opts -> Just opts {outputPath = Just d}
          )
          "(FILE or DIRECTORY)"
      )
      "specifies output path or directory"
  ]

compilerOpts :: [String] -> IO Options
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> case repeatedlyApply o defaultOptions of
      Just o' -> return o' {files = n}
      Nothing -> fail "Invalid combination of commands"
    (_, _, errs) -> fail (concat errs ++ usageInfo header options ++ usedEnvars)
  where
    header = "Usage: lc [OPTION...] files..."
    usedEnvars =
      [fmtTrim|
          Environment Variables:
            LC_IMPORT_PATH            Directories where `import` will search for files
        |]
    repeatedlyApply :: [a -> Maybe a] -> a -> Maybe a
    repeatedlyApply [] x = Just x
    repeatedlyApply (f : xs) x = repeatedlyApply xs =<< f x
