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
      Parser, option, ReadM, eitherReader, showDefaultWith )
import System.FilePattern (FilePattern)
import Web.Tailwind
    ( Mode(..),
      tailwindConfigContent,
      tailwindConfig,
      tailwindMode,
      tailwindOutput,
      runTailwind,
      Plugin(..),
      tailwindConfigPlugins )
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega

data Cli = Cli
  { content :: NonEmpty FilePattern,
    output :: FilePath,
    mode :: Mode,
    plugins :: [Plugin]
  }
  -- deriving (Eq, Show)
  deriving (Show)

cliParser :: Parser Cli
cliParser = do
  content <- NE.some (argument str (metavar "SOURCES..."))
  output <- strOption
    (  long "output"
    <> short 'o'
    <> metavar "OUTPUT"
    <> value "tailwind.css"
    )
  mode <- modeParser
  plugins <- pluginsParser
  pure Cli {..}

modeParser :: Parser Mode
modeParser = do
  bool Production JIT <$> switch (long "watch" <> short 'w' <> help "Run JIT")

pluginsParser :: Parser [Plugin]
pluginsParser = option megaparsecPlugins
    (  long "plugins"
    <> short 'p'
    <> value [Typography, Forms, LineClamp, AspectRatio]
    <> showDefaultWith (intercalate "," . fmap show)
    <> help "specify enabled plugins, \"\" for none"
    )
  where
    megaparsecPlugins = megaparsecReader $ Mega.sepBy plugin sep

    sep = Mega.hspace *> Mega.char ',' *> Mega.hspace
    plugin = asum $ (\(s, t) -> Mega.string' s $> t) <$>
      [ ("typography"  , Typography )
      , ("forms"       , Forms      )
      , ("line-clamp"  , LineClamp  )
      , ("aspect-ratio", AspectRatio)
      ]

    megaparsecReader :: Mega.Parsec Void Text a -> ReadM a
    megaparsecReader p = eitherReader
      $ first Mega.errorBundlePretty . Mega.parse p "<optparse-input>" . toText

main :: IO ()
main = do
  withUtf8 $ do
    cli <- execParser opts
    print cli
    runStdoutLoggingT $
      runTailwind $
        def
          & tailwindConfig % tailwindConfigContent .~ toList (content cli)
          & tailwindConfig % tailwindConfigPlugins .~ plugins cli
          & tailwindOutput .~ output cli
          & tailwindMode .~ mode cli
  where
    opts =
      info
        (cliParser <**> helper)
        ( fullDesc
            <> progDesc "Run Tailwind"
        )
