{-# LANGUAGE BlockArguments #-}

module LCDiagram.Bytecode.Execution where

import Control.Lens
import Data.Map qualified as M
import LCDiagram.Bytecode.Compiler (compileFile)
import LCDiagram.Bytecode.Interpreter
import LCDiagram.Bytecode.Types

runLC :: (Show a, Num a) => FilePath -> SymbolTable a -> IO a
runLC mainFileLoc table = do
  imports' <- case table M.!? "imports" of
    Just (Function (FnVals x [])) -> return x
    _ -> return []
  main' <- case table M.!? "main" of
    Nothing -> fail "No `main` function defined"
    Just x -> return x

  stdLib <- compileFile "std"

  let traceRead =
        [ ("trace", Function $ FnVals {code = [Trace], captures = []})
        , ("read", Function $ FnVals {code = [Read], captures = []})
        ]

  evalStateT
    exec
    VMState
      { succesor = (+ 1)
      , stack =
          [ StackFrame
              { symbols = []
              , instructions =
                  main'
                    & #_Function . #code <>~ [Call]
                    & #_Function . #code %~ (imports' <>)
              , content = [Succesor (+ 1), Value 0]
              }
          ]
      , input = 0
      , globals = M.delete "imports" (M.delete "main" table) <> stdLib <> traceRead
      , mainFileDir = mainFileLoc
      }
