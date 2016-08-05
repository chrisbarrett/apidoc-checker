-- |Implements verification of apidoc specs.
module Apidoc.Check
  (
    module Exports
  , validate
  ) where

import           Apidoc.Check.Env as Exports (Env)
import           Apidoc.Check.Err as Exports (Err)
import qualified Apidoc.DSL       as DSL
import           Apidoc.Json
import           Data.Sequence    (Seq)

validate :: Env -> Json -> (Seq Err, Maybe DSL.Spec)
validate = undefined
