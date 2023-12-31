{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LCDiagram.Bytecode.Interpreter (exec, VMState (..), VM) where

import Control.Lens
import Control.Monad.Catch
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Sequence qualified as S
import Data.Sequence.Lens (viewL)
import Data.Text (replace)
import GHC.IO.Handle (hSetEcho)
import LCDiagram.Bytecode.Compiler (compileFile)
import LCDiagram.Bytecode.Types
import PyF
import System.FilePath ((</>))
import System.IO (getChar)

data VMState a = VMState
  { input :: a
  , stack :: Seq (StackFrame a)
  , succesor :: a -> a
  , globals :: SymbolTable a
  , mainFileDir :: FilePath
  }
  deriving stock (Generic)

type VM m a = (MonadState (VMState a) m, MonadFail m, MonadIO m, MonadMask m, Show a)

getInstruction :: VM m a => m Instruction
getInstruction = do
  gets (^? #stack . _head . #instructions . #_Function . #code . _head) >>= \case
    Just x -> return x
    Nothing -> fail "Tried to run an instruction where there is none"
{-# INLINE getInstruction #-}

pop :: VM m a => m (Function a)
pop = do
  gets (^? #stack . _head . #content . viewL) >>= \case
    Just (x S.:< xs) -> do
      #stack . _head . #content .= xs
      return x
    Just S.EmptyL -> fail "Empty Stack Frame Contents"
    Nothing -> fail "Empty Stack"
{-# INLINE pop #-}

push :: VM m a => Function a -> m ()
push x = do
  #stack . _head . #content %= (x <|)
{-# INLINE push #-}

advance :: VM m a => m a
advance = do
  gets (fmap S.viewl . (^? #stack . _head . #instructions . #_Function . #code)) >>= \case
    Just (_ S.:< S.Empty) -> do
      returnVal <- pop
      gets (S.viewl . stack) >>= \case
        (_ S.:< S.Empty) -> do
          case returnVal of
            Value x -> return x
            _ -> do
              stack' <- gets stack
              fail $
                "Main function does not return a church numeral\nreturned: "
                  <> show returnVal
                  <> "\nstack: "
                  <> show stack'
        (_ S.:< xs) -> do
          #stack .= xs
          push returnVal
          advance
        S.EmptyL -> error "This Probably Should not occur"
    Just (_ S.:< xs) -> do
      #stack . _head . #instructions . #_Function . #code .= xs
      exec
    _ -> fail "Tried to advance but no next instruction"

_logVM :: forall a m. (VM m a) => m ()
_logVM = do
  VMState {stack} <- get
  putStrLn "  Stacks:"
  putTextLn . addTabs . unlines . toList $ S.mapWithIndex prettifyFrame (S.reverse stack)
  putStrLn "  Symbols:"
  putTextLn . addTabs . unlines . toList $ fmap onSymbols (S.reverse stack)
  Just i <- gets (^? #stack . _head . #instructions . #_Function . #code . _head)
  putStrLn $ "next instruction: " <> show i
  where
    onSymbols StackFrame {symbols}
      | M.null symbols = ""
      | otherwise = foldr ((\x y -> x <> "\n\t" <> y) . prettifySymbols) "" (M.toList symbols)
    prettifySymbols (k, v) = k <> ": " <> Prelude.show v
    prettifyFrame i StackFrame {content} =
      show i <> ": " <> show (toList content)
    addTabs :: Text -> Text
    addTabs = ("\t" <>) . replace "\n" "\n\t"

exec :: (VM m a) => m a
exec = do
  getInstruction >>= \case
    Call -> do
      pop
        >>= \case
          Succesor f -> do
            arg <-
              pop >>= \case
                Value x -> return x
                _ -> fail "Succesor argument should only be a Value type"
            push (Value (f arg))
            advance
          Value _ -> fail "Unable to call a value"
          f@(Function _) -> do
            arg <- pop
            #stack %= (StackFrame {content = [], symbols = [], instructions = f} <|)
            push arg
            exec
    Store name -> do
      v <- pop
      #stack . _head . #symbols %= M.insertWith const name v
      advance
    MakeClosure valName -> do
      closure <- pop
      let fromStackFrame = #stack . _head . #symbols . ix valName
          fromClosure = #stack . _head . #instructions . #_Function . #captures . ix valName
          fromGlobal = #globals . ix valName
      gets (^? (fromGlobal `failing` fromStackFrame `failing` fromClosure))
        >>= \case
          Just v -> push (closure & #_Function . #captures %~ M.insert valName v)
          Nothing -> fail [fmt|Local Variable '{valName}' does not exist|]
      advance
    Load name -> do
      let getStackFrame = gets (^? #stack . _head . #symbols . ix name)
          getFromClosure = gets (^? #stack . _head . #instructions . #_Function . #captures . ix name)
          getGlobal = gets (^? #globals . ix name)
      liftA3 (\x y z -> x <|> y <|> z) getStackFrame getFromClosure getGlobal
        >>= \case
          Just x -> push x
          Nothing -> fail [fmt|Variable '{name}' does not exist|]
      advance
    Trace -> do
      pop >>= \x -> case x of
        Value val' -> putStrLn $ "Value " <> show val'
        Succesor _ -> putStrLn "<<SUCCESOR>>"
        f -> do
          globals <- gets globals
          input <- gets input
          succesor <- gets succesor
          mainFileDir <- gets mainFileDir
          (>>= print) $
            evalStateT
              exec
              VMState
                { input
                , succesor
                , stack =
                    [ StackFrame
                        { symbols = []
                        , instructions = f & #_Function . #code <>~ [Call]
                        , content = [Succesor succesor, Value input]
                        }
                    ]
                , globals
                , mainFileDir
                }
          push x
      advance
    Import file -> do
      fileDir <- gets mainFileDir
      syms <- liftIO $ compileFile (fileDir </> file)
      #globals <>= syms
      advance
    Read -> do
      liftIO $ hSetEcho stdin False
      c <- fromEnum <$> liftIO getChar
      liftIO $ hSetEcho stdin True
      x' <- uniqueName "#read."
      #globals %= M.insert x' (Function (FnVals (fromList $ concat $ replicate c [Load "f", Call]) []))
      push (Function (FnVals [Store "f", Load x', MakeClosure "f"] []))
      advance
      where
        uniqueName name = do
          fromStackFrame <- gets (^. #stack . _head . #symbols)
          fromClosure <- gets (^. #stack . _head . #instructions . #_Function . #captures)
          fromGlobal <- gets (^. #globals)
          if M.member name fromStackFrame || M.member name fromClosure || M.member name fromGlobal
            then uniqueName (name <> "_.")
            else return name
