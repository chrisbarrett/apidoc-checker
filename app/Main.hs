{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Apidoc.Parser                as Parser
import qualified Data.ByteString              as BS
import           Options.Applicative
import qualified Path
import qualified System.Environment           as Environment
import qualified System.Exit                  as Exit
import qualified Text.PrettyPrint.ANSI.Leijen as PP

main :: IO ()
main = do
    prog <- Environment.getProgName
    Opts {..} <- execParser (options prog)
    absPath <- Path.toFilePath <$> Path.parseAbsFile optFile
    result <- Parser.parse <$> BS.readFile absPath
    case result of
        Right _  ->
            putStrLn "Parsed with no errors."
        Left errs -> do
            PP.putDoc errs
            Exit.exitFailure

-- * Option parsing

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
