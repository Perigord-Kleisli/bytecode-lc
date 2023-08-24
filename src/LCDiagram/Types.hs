module LCDiagram.Types (LCError (..)) where

import Text.Megaparsec.Error
import GHC.Show (Show(..))

data LCError
  = LCParseError (ParseErrorBundle Text Void)
  | LCOParseError (ParseErrorBundle ByteString Void)
  | forall e. (Exception e) => OtherError e

instance Show LCError where
  show (LCParseError e) = displayException e
  show (LCOParseError e) = displayException e
  show (OtherError e) = displayException e

instance Exception LCError where
