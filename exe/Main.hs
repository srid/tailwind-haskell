{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import Control.Monad.Logger.CallStack (runStdoutLoggingT)
import Data.Default (Default (def))
import Main.Utf8 (withUtf8)
import Optics.Core ((%), (.~))
import Options.Applicative
    ( argument,
      fullDesc,
      help,
      info,
      long,
      metavar,
      progDesc,
      short,
      str,
      strOption,
      switch,
      value,
      execParser,
      helper,
      Parser, showDefaultWith )
import System.FilePattern (FilePattern)
import Web.Tailwind
    ( Mode(..),
      tailwindConfigContent,
      tailwindConfig,
      tailwindMode,
      tailwindOutput,
      runTailwind,
      tailwindConfigPlugins, tailwindConfigTheme )
import qualified Data.Text as Text
import Data.Traversable (for)
import Optics.Setter (set)
import qualified Data.Text.IO as Text

data Cli = Cli
  { content :: NonEmpty FilePattern,
    output :: FilePath,
    mode :: Mode,
    plugins :: Text,
    themeJson :: Maybe FilePath
  }
  deriving (Eq, Show)

cliParser :: Parser Cli
cliParser = do
  content   <- NE.some   (argument str (metavar "SOURCES..."))
  output    <- strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> value "tailwind.css")
  mode      <- modeParser
  plugins   <- Text.pack <$> strOption
    (  long "plugins"
    <> short 'p'
    <> value "typography,forms,line-clamp,aspect-ratio"
    <> showDefaultWith id
    <> help "specify enabled plugins"
    )
  themeJson <- optional $ strOption
    (  long "theme"
    <> short 't'
    <> help "json file with theme object"
    )
  pure Cli {..}

modeParser :: Parser Mode
modeParser = do
  bool Production JIT <$> switch (long "watch" <> short 'w' <> help "Run JIT")

main :: IO ()
main = do
  withUtf8 $ do
    cli <- execParser opts
    print cli
    mTheme <- traverse Text.readFile $ themeJson cli
    runStdoutLoggingT $
      runTailwind $
        def
          & tailwindConfig % tailwindConfigContent .~ toList (content cli)
          & tailwindConfig % tailwindConfigPlugins .~ readPlugins (plugins cli)
          & maybe id (set $ tailwindConfig % tailwindConfigTheme) mTheme
          & tailwindOutput .~ output cli
          & tailwindMode .~ mode cli
  where
    opts =
      info
        (cliParser <**> helper)
        ( fullDesc
            <> progDesc "Run Tailwind"
        )
    readPlugins txt =
      let ePlugins = for (Text.splitOn "," txt) \strPlugin ->
            case readMaybe $ Text.unpack $ Text.toLower $ Text.strip strPlugin of
              Just plugin -> pure plugin
              Nothing     -> Left $ "Could not parse " <> strPlugin
      in  either error id ePlugins
