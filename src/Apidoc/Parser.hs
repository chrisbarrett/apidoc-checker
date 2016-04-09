module Apidoc.Parser  where

import           Apidoc.Spec
import qualified Control.Arrow        as Arrow
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Text            (Text)
import qualified Data.Text            as Text

data Error = Error Text
  deriving (Show)

parse :: BS.ByteString -> Either Error Service
parse json = Arrow.left (Error . Text.pack) (Aeson.eitherDecode json)
