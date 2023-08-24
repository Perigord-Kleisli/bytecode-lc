module LCDiagram.Repl.Types (ReplState (..), Repl, ReplSt) where

import Data.Digest.Pure.MD5 (MD5Digest)
import LCDiagram.Bytecode.Interpreter (VMState)
import System.Console.Haskeline (InputT)

data ReplState a = ReplState
  { vm :: VMState a
  , loadedFiles :: Map FilePath MD5Digest
  }
  deriving stock (Generic)

type ReplSt a = StateT (ReplState a) IO
type Repl a = InputT (StateT (ReplState a) IO)
