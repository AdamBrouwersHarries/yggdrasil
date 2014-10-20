{-# LANGUAGE TemplateHaskell #-}
module EmbedStr (embedStr, fileAsString) where

-- derived from: http://www.haskell.org/pipermail/haskell-cafe/2008-September/047384.html

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Text.CSL.Style
import Text.CSL.Parser
import Data.FileEmbed
import qualified Data.ByteString

embedStr :: IO String -> ExpQ
embedStr readStr = lift =<< runIO readStr

-- | Loads the content of a file as a string constant expression.
-- The given path is relative to the source directory.
fileAsString :: FilePath -> Q Exp
fileAsString = do
  -- addDependentFile path -- works only with template-haskell >= 2.7
  stringE . T.unpack . T.strip <=< runIO . T.readFile
