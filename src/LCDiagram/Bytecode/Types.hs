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
  | Func Text (Seq Instruction)
  | Import FilePath
  deriving stock (Show)

type SymbolTable a = Map Text (Function a)

data FnVals a = FnVals {code :: Seq Instruction, captures :: SymbolTable a} deriving stock Generic

data Function a
  = Function (FnVals a)
  | Succesor (a -> a)
  | Value a
  deriving stock (Generic)

instance Show a => Show (Function a) where
  -- show (Function {code, closure}) = [fmt|Function {{code={Text.Show.show code},closure={Text.Show.show closure}}} |]
  -- show (Function {code, closure}) = "Function {" <>
  --   "\n  Instructions: \n\t"
  --     <> foldr (\x y -> x <> "\n\t" <> y) "" (fmap Prelude.show code.program)
  --     <> ( if M.null closure
  --           then ""
  --           else
  --             "\n  Captured Values: \n\t"
  --               <> foldr ((\x y -> x <> "\n\t" <> y) . prettifySymbols) "" (M.toList closure)
  --        )
  --     <> "\n}"
  --   where
  --     prettifySymbols :: (Text, Function a) -> String
  --     prettifySymbols (k, v) = toString k <> ": " <> Prelude.show v
  --     prettifyFrame i StackFrame {content} =
  --       Prelude.show i <> ": " <> Prelude.show (toList content)
  show (Function (FnVals{code}) ) = "<<" <> Prelude.show (toList code) <> ">>"
  show (Value a) = "Value " ++ Prelude.show a
  show (Succesor _) = "<<SUCCESOR>>"

data StackFrame a = StackFrame
  { content :: Seq (Function a)
  , symbols :: SymbolTable a
  , instructions :: Function a
  }
  deriving stock (Show, Generic)
