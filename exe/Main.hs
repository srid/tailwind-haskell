{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

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
      Parser )
import System.FilePattern (FilePattern)
import Web.Tailwind
    ( Mode(..),
      tailwindConfigContent,
      tailwindConfig,
      tailwindMode,
      tailwindOutput,
      runTailwind,
      OfficialPlugin(PluginAspectRatio, PluginTypography, PluginForms,
                     PluginLineClamp),
      tailwindConfigPlugins, tailwindConfigTheme )
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

data Cli = Cli
  { content :: NonEmpty FilePattern,
    output :: FilePath,
    mode :: Mode,
    bPluginTypography  :: Bool,
    bPluginForms       :: Bool,
    bPluginLineClamp   :: Bool,
    bPluginAspectRatio :: Bool,
    bAllPlugins        :: Bool,
    themeJson :: FilePath
  }
  deriving (Eq, Show)

cliParser :: Parser Cli
cliParser = do
  content            <- NE.some (argument str (metavar "SOURCES..."))
  output             <- strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> value "tailwind.css")
  mode               <- modeParser
  bPluginTypography  <- switch (long "plugin-typography"   <> help "enable official typography plugin")
  bPluginForms       <- switch (long "plugin-forms"        <> help "enable official forms plugin")
  bPluginLineClamp   <- switch (long "plugin-line-clamp"   <> help "enable official line-clamp plugin")
  bPluginAspectRatio <- switch (long "plugin-aspect-ratio" <> help "enable official aspect ratio plugin")
  bAllPlugins        <- switch (long "all-plugins" <> short 'a' <> help "enable all official plugins")
  themeJson          <- strOption (long "theme" <> short 't' <> help "json file with theme object")
  pure Cli {..}

modeParser :: Parser Mode
modeParser = do
  bool Production JIT <$> switch (long "watch" <> short 'w' <> help "Run JIT")

main :: IO ()
main = do
  withUtf8 $ do
    cli <- execParser opts
    print cli
    theme <- getObject $ themeJson cli
    runStdoutLoggingT $
      runTailwind $
        def
          & tailwindConfig % tailwindConfigContent .~ toList (content cli)
          & tailwindConfig % tailwindConfigPlugins .~ plugins cli
          & tailwindConfig % tailwindConfigTheme   .~ theme
          & tailwindOutput .~ output cli
          & tailwindMode .~ mode cli
  where
    opts =
      info
        (cliParser <**> helper)
        ( fullDesc
            <> progDesc "Run Tailwind"
        )
    plugins Cli{..} =
      if bAllPlugins
      then [PluginTypography, PluginForms, PluginLineClamp, PluginAspectRatio]
      else
           bool [] [PluginTypography ] bPluginTypography
        <> bool [] [PluginForms      ] bPluginForms
        <> bool [] [PluginLineClamp  ] bPluginLineClamp
        <> bool [] [PluginAspectRatio] bPluginAspectRatio

    getObject filename = Aeson.eitherDecodeFileStrict' filename >>= \case
      Right o   -> pure o
      Left  msg -> error $ "Could not decode file "
                         <> Text.pack filename <> ":"
                         <> Text.pack (show msg)
