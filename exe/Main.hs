{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import Control.Lens.Operators
import Control.Monad.Logger.CallStack (runStdoutLoggingT)
import Data.Default (Default (def))
import Main.Utf8 (withUtf8)
import Options.Applicative
import System.FilePattern (FilePattern)
import Web.Tailwind

data Cli = Cli
  { content :: NonEmpty FilePattern,
    output :: FilePath,
    mode :: Mode
  }
  deriving (Eq, Show)

cliParser :: Parser Cli
cliParser = do
  content <- NE.some (argument str (metavar "SOURCES..."))
  output <- strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> value "tailwind.css")
  mode <- modeParser
  pure Cli {..}

modeParser :: Parser Mode
modeParser = do
  bool Production JIT <$> switch (long "watch" <> short 'w' <> help "Run JIT")

main :: IO ()
main = do
  withUtf8 $ do
    cli <- execParser opts
    print cli
    runStdoutLoggingT $
      runTailwind $
        def
          & tailwindConfig . tailwindConfigContent .~ toList (content cli)
          & tailwindOutput .~ output cli
          & tailwindMode .~ mode cli
  where
    opts =
      info
        (cliParser <**> helper)
        ( fullDesc
            <> progDesc "Run Tailwind"
        )
