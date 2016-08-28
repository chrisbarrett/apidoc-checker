{-# LANGUAGE FlexibleContexts  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Apidoc.Check                 as Check
import qualified Apidoc.Json                  as Json
import qualified Apidoc.Render                as Render
import           Control.Monad.Reader         as Reader
import qualified Data.Bifunctor               as Bifunctor
import           Options.Applicative          as Options
import qualified System.Environment           as Environment
import qualified System.Exit                  as Exit
import qualified System.IO                    as IO
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Opts = Opts {
    optFile     :: FilePath
  , optNoColour :: Bool
  }

desc prog = fullDesc
    <> header (prog ++ " - apidoc spec validator")
    <> progDesc "Validates an apidoc JSON specification"

flags = Opts
    <$> argument str (metavar "FILE" <> help "Path to the apidoc spec to validate")
    <*> switch (long "plain" <> help "Remove ANSI colour codes from output.")


main :: IO ()
main = do
    prog <- Environment.getProgName
    os <- Options.execParser (info (helper <*> flags) (desc prog))
    Reader.runReaderT (validateFile >>= either reject accept) os
  where
    accept _ = do
        printDoc IO.stdout (PP.dullgreen "Validation succeeded.")
        liftIO Exit.exitSuccess

    reject errs = do
        printDoc IO.stderr errs
        printDoc IO.stdout (PP.line <> PP.red "Apidoc spec was malformed. Validation failed.")
        liftIO Exit.exitFailure

    validateFile = do
        Opts {optFile = f} <- Reader.ask
        js <- liftIO (Json.parseFile f)
        pure (js >>= Bifunctor.first Render.renderErrs . Check.validate)

    printDoc h d = do
        Opts {optNoColour = noColour} <- Reader.ask
        let d' = if noColour then PP.plain d else d
        liftIO (PP.displayIO h (PP.renderPretty 1 80 d'))
