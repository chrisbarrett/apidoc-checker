{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Apidoc.Check                 as Check
import qualified Apidoc.Json                  as Json
import qualified Apidoc.Render                as Render
import qualified Data.Bifunctor               as Bifunctor
import qualified Data.ByteString              as Strict
import           Options.Applicative          as Options
import qualified System.Environment           as Environment
import qualified System.Exit                  as Exit
import           System.IO                    (stderr, stdout)
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Opts = Opts {
    optFile     :: FilePath
  , optNoColour :: Bool
  }

desc prog = fullDesc
    <> header (prog ++ " - apidoc spec validator")
    <> progDesc "Validates an apidoc JSON specification"

opts = Opts
    <$> argument str (metavar "FILE" <> help "Path to the apidoc spec to validate")
    <*> switch (long "plain" <> help "Remove ANSI colour codes from output.")


main :: IO ()
main = do
    prog <- Environment.getProgName
    Opts {optFile = f, .. } <- Options.execParser (info (helper <*> opts) (desc prog))

    let printDoc h d = PP.displayIO h (PP.renderPretty 1 10000 (withColour d))
        withColour d = if optNoColour then PP.plain d else d

    validateFile f >>= \case
      Right _ -> do
          printDoc stdout (PP.dullgreen "Finished with no errors.")
          Exit.exitSuccess
      Left errs -> do
          printDoc stderr errs
          printDoc stdout (PP.red "Malformed apidoc spec.")
          Exit.exitFailure

  where
    validateFile f = do
        bs <- Strict.readFile f
        js <- Json.parseFile f
        pure (js >>= Bifunctor.first (Render.renderErrs bs) . Check.validate)
