{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tailwind runner in Haskell
module Web.Tailwind
  ( -- * Runner
    runTailwind,

    -- * Types
    TailwindConfig (..),
    Tailwind (..),
    Css (..),
    Mode (..),
    Plugin (..),
    tailwindConfig,
    tailwindInput,
    tailwindOutput,
    tailwindConfigContent,
    tailwindConfigPlugins,
    tailwindConfigTheme,
    tailwindMode,
  )
where

import Control.Monad.Logger (MonadLogger, logInfoN)
import Data.ByteString (hPut)
import Data.Default (Default (def))
import NeatInterpolation (text)
import Optics.TH (makeLenses)
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist)
import System.IO (hClose)
import System.Which (staticWhich)
import Text.Printf (printf)
import qualified Text.Show
import UnliftIO (MonadUnliftIO, finally)
import UnliftIO.Directory (removeFile)
import UnliftIO.Process (callProcess)
import UnliftIO.Temporary (withSystemTempFile)
import qualified Text.Read
import Text.ParserCombinators.ReadP (string, choice)
import qualified Data.Text as Text

data Plugin
  = Typography
  | Forms
  | LineClamp
  | AspectRatio

instance Text.Show.Show Plugin where
  show = \case
    Typography  -> "typography"
    Forms       -> "forms"
    LineClamp   -> "line-clamp"
    AspectRatio -> "aspect-ratio"

instance Text.Read.Read Plugin where
  readPrec = Text.Read.lift $ choice
    [ string "typography"   $> Typography
    , string "forms"        $> Forms
    , string "line-clamp"   $> LineClamp
    , string "aspect-ratio" $> AspectRatio
    ]

-- | Haskell version of tailwind.config.js
--
-- Only the subset we care to define, as some fields (eg: plugins) are defined
-- with arbitrary JS code.
data TailwindConfig = TailwindConfig
  { -- | List of source patterns that reference CSS classes
    _tailwindConfigContent :: [FilePath],
    _tailwindConfigPlugins :: [Plugin],
    _tailwindConfigTheme   :: Text
  }

newtype Css = Css {unCss :: Text}

data Mode = JIT | Production
  deriving (Eq, Show)

data Tailwind = Tailwind
  { _tailwindConfig :: TailwindConfig,
    _tailwindInput :: Css,
    _tailwindOutput :: FilePath,
    _tailwindMode :: Mode
  }

makeLenses ''TailwindConfig
makeLenses ''Tailwind

instance Default TailwindConfig where
  def =
    TailwindConfig
      { _tailwindConfigContent = []
      , _tailwindConfigPlugins =
          [ Typography
          , LineClamp
          , Forms
          , AspectRatio
          ]
      , _tailwindConfigTheme = ""
      }

instance Default Tailwind where
  def =
    Tailwind
      { _tailwindConfig = def,
        _tailwindInput = def,
        _tailwindOutput = "tailwind.css",
        _tailwindMode = JIT
      }

instance Default Css where
  def =
    Css
      [text|
      @tailwind base;
      @tailwind components;
      @tailwind utilities;
      |]

instance Text.Show.Show TailwindConfig where
  show TailwindConfig{..} =
    let content = Text.pack $ show _tailwindConfigContent
        mkStrPlugin plugin = "require('@tailwindcss/" <> show plugin <> "')"
        strPlugins = Text.intercalate ",\n" $ mkStrPlugin <$> _tailwindConfigPlugins
        theme = if Text.null _tailwindConfigTheme
          then ""
          else "\ntheme: " <> Text.strip _tailwindConfigTheme <> ",\n"
     in toString
          [text|
          const defaultTheme = require('tailwindcss/defaultTheme')
          const colors = require('tailwindcss/colors')

          module.exports = {
            content: ${content},${theme}
            plugins: [
              ${strPlugins}
            ]
          }
          |]

tailwind :: FilePath
tailwind = $(staticWhich "tailwind")

modeArgs :: Mode -> [String]
modeArgs = \case
  JIT -> ["-w"]
  Production -> ["--minify"]

runTailwind :: (MonadUnliftIO m, MonadLogger m, HasCallStack) => Tailwind -> m ()
runTailwind Tailwind {..} = do
  withTmpFile (show _tailwindConfig) $ \configFile ->
    withTmpFile (unCss _tailwindInput) $ \inputFile ->
      let f = bool id (failIfFileNotCreated _tailwindOutput) (_tailwindMode == Production)
       in f $ callTailwind $ ["-c", configFile, "-i", inputFile, "-o", _tailwindOutput] <> modeArgs _tailwindMode
  when (_tailwindMode == JIT) $
    error "Tailwind exited unexpectedly!"

withTmpFile :: MonadUnliftIO m => Text -> (FilePath -> m a) -> m a
withTmpFile s f = do
  withSystemTempFile "ema-tailwind-tmpfile" $ \fp h -> do
    liftIO $ do
      putStrLn $ "$ cat " <> fp
      putTextLn s
      hPut h (encodeUtf8 s) >> hClose h
    f fp
      `finally` removeFile fp

-- Workaround for https://github.com/srid/emanote/issues/232
--
-- If the given IO action doesnot create this file (we remove the file before
-- running the IO action), then fail with `error`.
failIfFileNotCreated :: (MonadUnliftIO m, HasCallStack) => FilePath -> m a -> m a
failIfFileNotCreated fp m = do
  liftIO (doesFileExist fp) >>= \case
    True -> removeFile fp
    _ -> pure ()
  x <- m
  exists <- liftIO $ doesFileExist fp
  if exists
    then pure x
    else error $ "File not created: " <> toText fp

callTailwind :: (MonadIO m, MonadLogger m) => [String] -> m ()
callTailwind args = do
  logInfoN $ "Running Tailwind at " <> toText tailwind <> " with args: " <> show args
  liftIO (doesFileExist tailwind) >>= \case
    True ->
      timeIt $ do
        callProcess tailwind args
    False ->
      error $ "Tailwind compiler not found at " <> toText tailwind

timeIt :: MonadIO m => m b -> m b
timeIt m = do
  t0 <- liftIO getCPUTime
  !x <- m
  t1 <- liftIO getCPUTime
  let diff :: Double = fromIntegral (t1 - t0) / (10 ^ (9 :: Integer))
  liftIO $ printf "Process duration: %0.3f ms\n" diff
  pure $! x
