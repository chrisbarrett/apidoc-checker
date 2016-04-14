{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Apidoc.Parser                as Parser
import qualified Data.Validation              as Validation
import           Options.Applicative
import qualified System.Environment           as Environment
import qualified System.Exit                  as Exit
import           System.IO                    (stderr, stdout)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

main :: IO ()
main = do
    prog <- Environment.getProgName
    Opts {..} <- execParser (options prog)

    let printDoc h d =
            PP.displayIO h (PP.renderPretty 1 10000 (if optNoColour then PP.plain d else d))

    Parser.parseFile optFile >>= \case
        Validation.Success _  ->
            printDoc stdout (PP.dullgreen "Finished with no errors.")
        Validation.Failure errs -> do
            printDoc stderr errs
            printDoc stdout (PP.red "Malformed apidoc spec.")
            Exit.exitFailure

-- * Option parsing

data Opts = Opts {
    optFile     :: FilePath
  , optNoColour :: Bool
  }

options :: String -> ParserInfo Opts
options prog = info (helper <*> opts) desc
    where
      desc = fullDesc
          <> header (prog ++ " - apidoc spec validator")
          <> progDesc "Validates an apidoc JSON specification"
      opts =
          Opts <$> argument str (metavar "FILE" <> help "Path to the apidoc spec to validate")
               <*> switch (long "plain" <> help "Remove ANSI colour codes from output.")
