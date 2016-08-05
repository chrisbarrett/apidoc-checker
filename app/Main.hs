{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Apidoc.Check                 as Check
import qualified Apidoc.Err                   as Err
import qualified Apidoc.Json                  as Json
import           Control.Monad                (forM)
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

    Json.parseFile optFile >>= \case
        Right js -> do
            let (errs, res) = Check.validate mempty js
            forM errs $
              printDoc stderr . Err.render
            case res  of
              Just _ -> pure ()
              Nothing -> do
                printDoc stdout (PP.red "Malformed apidoc spec.")
                Exit.exitFailure

        Left errs -> do
            printDoc stderr errs
            printDoc stdout (PP.red "Malformed apidoc spec.")
            Exit.exitFailure

    printDoc stdout (PP.dullgreen "Finished.")

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
