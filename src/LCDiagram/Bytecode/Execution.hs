{-# LANGUAGE BlockArguments #-}

module LCDiagram.Bytecode.Execution where

import Control.Lens
import Data.Map qualified as M
import LCDiagram.Bytecode.Interpreter
import LCDiagram.Bytecode.Types

runLC :: (Show a, Num a) => SymbolTable a -> IO a
runLC table = do
  main' <- case table M.!? "main" of
    Nothing -> fail "No `main` function defined"
    Just x -> return x

  evalStateT
    exec
    VMState
      { succesor = (+ 1)
      , stack =
          [ StackFrame
              { symbols = []
              , instructions = main' & #_Function . #code <>~ [Call]
              , content = [Succesor (+ 1), Value 0]
              }
          ]
      , input = 0
      , globals = M.delete "main" table
      }
