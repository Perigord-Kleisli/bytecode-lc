{-# LANGUAGE NamedFieldPuns #-}

module LCDiagram.Bytecode.Types (
  Instruction (..),
  SymbolTable,
  Function (..),
  FnVals (..),
  StackFrame (..),
) where

import Text.Show (Show (show))

data Instruction
  = Call
  | Trace
  | Read
  | MakeClosure Text -- `Text` partains to the variable in the symbol table to capture
  | Store Text
  | Load Text
  | Import FilePath
  deriving stock (Show)

type SymbolTable a = Map Text (Function a)

data FnVals a = FnVals {code :: Seq Instruction, captures :: SymbolTable a} deriving stock (Generic)

data Function a
  = Function (FnVals a)
  | Succesor (a -> a)
  | Value a
  deriving stock (Generic)

instance Show (Function a) where
  show (Function (FnVals {code})) = "<<" <> Prelude.show (toList code) <> ">>"
  show (Value _) = "Value"
  show (Succesor _) = "<<SUCCESOR>>"

data StackFrame a = StackFrame
  { content :: Seq (Function a)
  , symbols :: SymbolTable a
  , instructions :: Function a
  }
  deriving stock (Show, Generic)
