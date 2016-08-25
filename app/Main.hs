{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Apidoc.Check                 as Check
import qualified Apidoc.Json                  as Json
import qualified Apidoc.Render                as Render
import           Control.Lens                 hiding (argument)
import           Control.Monad                (forM_)
import qualified Data.ByteString              as Strict
import qualified Data.Sequence                as Seq
import           Options.Applicative
import qualified System.Environment           as Environment
import qualified System.Exit                  as Exit
import           System.IO                    (stderr, stdout)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

main :: IO ()
main = do
    prog <- Environment.getProgName
    Opts {..} <- execParser (options prog)

    let printDoc h d = PP.displayIO h (PP.renderPretty 1 10000 (withColour d))
        withColour d = if optNoColour then PP.plain d else d

    results <- runValidator optFile
    case results of
      Right errs -> do
        printDoc stderr errs
        printDoc stdout (PP.dullgreen "Finished.")
      Left errs -> do
        forM_ errs (printDoc stderr)
        printDoc stdout (PP.red "Malformed apidoc spec.")
        Exit.exitFailure

  where
    runValidator file = do
        bs <- Strict.readFile file
        js <- Json.parseFile file
        pure (bimap Seq.singleton (renderErrs bs . Check.validate) js)

    renderErrs s (Left errs)       = Render.renderErrs s errs
    renderErrs s (Right (errs, _)) = Render.renderErrs s errs



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
