{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Apidoc.Parser        as Parser
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.IO         as Text
import           Options.Applicative
import qualified Path
import qualified System.Environment   as Environment
import qualified System.Exit          as Exit

data Opts = Opts {
    optFile :: FilePath
  }

options :: String -> ParserInfo Opts
options prog = info (helper <*> opts) desc
    where
      desc = fullDesc
          <> header (prog ++ " - apidoc spec validator")
          <> progDesc "Validates an apidoc JSON specification"
      opts =
          Opts <$> argument str (metavar "FILE" <> help "Path to the apidoc spec to validate")

main :: IO ()
main = do
    prog <- Environment.getProgName
    Opts {..} <- execParser (options prog)
    absPath <- Path.toFilePath <$> Path.parseAbsFile optFile
    result <- Parser.parse <$> BS.readFile absPath

    case result of
        Right _  ->
            putStrLn "Parsed with no errors."
        Left (Parser.Error err) -> do
            Text.putStrLn err
            Exit.exitFailure
