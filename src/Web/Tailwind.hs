{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tailwind runner in Haskell
--
module Web.Tailwind
  ( -- * Runner
    runTailwind,

    -- * Types
    TailwindConfig (..),
    Tailwind (..),
    Mode (..),
    Plugin (..),
    tailwindConfig,
    tailwindInput,
    tailwindOutput,
    tailwindConfigContent,
    tailwindConfigPlugins,
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
import qualified Data.Text as Text

data Plugin
  = Typography
  | Forms
  | LineClamp
  | AspectRatio

instance Show Plugin where
  show = \case
    Typography  -> "typography"
    Forms       -> "forms"
    LineClamp   -> "line-clamp"
    AspectRatio -> "aspect-ratio"

-- | Haskell version of tailwind.config.js
--
data TailwindConfig = TailwindConfig
  { -- | List of source patterns that reference CSS classes
    _tailwindConfigContent :: [FilePath],
    -- | List of the "official tailwind" plugins,
    --   cf. https://tailwindcss.com/docs/plugins
    _tailwindConfigPlugins :: [Plugin]
  }
  deriving (Generic)

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
          , Forms
          , LineClamp
          , AspectRatio
          ]
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
    toString [text|
        module.exports = {
            content: ${content},${strPlugins}
        }
        |]
    where
      content = Text.pack $ show _tailwindConfigContent
      strPlugins = if null _tailwindConfigPlugins
        then ""
        else "\nplugins: [\n    "
          <> Text.intercalate ",\n    " (mkJSStrPlugin <$> _tailwindConfigPlugins)
          <> "\n]"
      mkJSStrPlugin plugin =
          "require('@tailwindcss/" <> Text.pack (show plugin) <> "')"

tailwind :: FilePath
tailwind = $(staticWhich "tailwindcss")

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
