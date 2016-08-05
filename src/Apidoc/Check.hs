-- |Implements verification of apidoc specs.
module Apidoc.Check where

import qualified Apidoc.DSL    as DSL
import           Apidoc.Env    (Env)
import           Apidoc.Err    (Err)
import qualified Apidoc.Err    as Err
import           Apidoc.Json
import qualified Apidoc.Json   as Json
import           Data.Sequence (Seq)

validate :: Env -> Json -> (Seq Err, Maybe DSL.Spec)
validate = undefined
