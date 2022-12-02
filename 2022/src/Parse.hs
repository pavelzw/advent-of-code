module Parse ( Parser ) where

import Data.Void (Void)
import qualified Data.Text as T
import Text.Megaparsec (Parsec)

type Parser = Parsec Void T.Text
