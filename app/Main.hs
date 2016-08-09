{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Apidoc.Check                 as Check
import qualified Apidoc.Check.Err             as Err
import qualified Apidoc.Json                  as Json
import           Control.Lens                 hiding (argument)
import           Control.Monad                (forM_)
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
      Right _ ->
        printDoc stdout (PP.dullgreen "Finished.")
      Left errs -> do
        forM_ errs (printDoc stderr)
        printDoc stdout (PP.red "Malformed apidoc spec.")
        Exit.exitFailure

  where
    runValidator file =
        bimap Seq.singleton (renderErrs . Check.validate)
          <$> Json.parseFile file

    renderErrs = over (_Left.traverse) Err.render



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
