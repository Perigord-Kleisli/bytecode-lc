{-# LANGUAGE OverloadedLists #-}

module LCDiagram.Bytecode.Compiler (lcCompiler) where

import Control.Lens
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Sequence qualified as Se
import LCDiagram.Bytecode.Types
import LCDiagram.Parser
import Shower (shower)

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
    lcDecCompiler (LCDiagram.Parser.Import _) = undefined
    lcDecCompiler (Def name expr) = do
      #focusedName .= name
      expr' <- lcExprCompiler expr
      #symbols %= M.insert name (Function $ FnVals (expr' Se.|> Call) [])

    uniqueName name = do 
      syms <- gets (^. #symbols)
      if M.member name syms 
        then  uniqueName (name <> "_")
        else return name

    lcExprCompiler :: LCExpr -> Compiler a (Seq Instruction)
    lcExprCompiler (Abs arg body) = do
      oldName <- gets focusedName
      lambdaName <- uniqueName (oldName <> "." <> arg)
      body' <- lcExprCompiler body
      #symbols %= M.insert lambdaName (Function $ FnVals ([Store arg] <> body') [])
      let caps = map MakeClosure $ capturedVals [arg] body
      return $ [Load lambdaName] <> fromList caps
    lcExprCompiler (App (Var "trace") arg) = do
      arg' <- lcExprCompiler arg
      return $ arg' <> [Trace, Call]
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
