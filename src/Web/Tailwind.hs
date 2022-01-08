{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tailwind runner in Haskell
--
-- TODO: Add `main` with CLI parser.
-- > tailwind-run [-w] 'src/**/*.hs'
module Web.Tailwind
  ( -- * Runner
    runTailwind,

    -- * Types
    TailwindConfig (..),
    Tailwind (..),
    Mode (..),
    tailwindConfig,
    tailwindInput,
    tailwindOutput,
    tailwindConfigContent,
    tailwindMode,
  )
where

import Control.Lens.TH (makeLenses)
import Control.Monad.Logger (MonadLogger, logInfoN)
import Data.Aeson (encode)
import Data.ByteString (hPut)
import Data.Default (Default (def))
import Deriving.Aeson
import NeatInterpolation (text)
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

-- | Haskell version of tailwind.config.js
--
-- Only the subset we care to define, as some fields (eg: plugins) are defined
-- with arbitrary JS code.
data TailwindConfig = TailwindConfig
  { -- | List of source patterns that reference CSS classes
    _tailwindConfigContent :: [FilePath]
  }
  deriving (Generic)
  deriving
    (ToJSON)
    via CustomJSON
          '[ FieldLabelModifier
               '[StripPrefix "_tailwindConfig", CamelToSnake]
           ]
          TailwindConfig

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
  show (decodeUtf8 . encode -> config) =
    -- Use `Object.assign` to merge JSON (produced in Haskell) with the rest of
    -- config (defined by raw JS; that cannot be JSON encoded)
    toString
      [text|
      module.exports =
        Object.assign(
          JSON.parse('${config}'),
          {
            theme: {
              extend: {},
            },
            plugins: [
              require('@tailwindcss/typography'),
              require('@tailwindcss/forms'),
              require('@tailwindcss/line-clamp'),
              require('@tailwindcss/aspect-ratio')
            ],
          })
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
      callTailwind $ ["-c", configFile, "-i", inputFile, "-o", _tailwindOutput] <> modeArgs _tailwindMode
  when (_tailwindMode == JIT) $
    error "Tailwind exited unexpectedly!"

withTmpFile :: MonadUnliftIO m => Text -> (FilePath -> m a) -> m a
withTmpFile s f = do
  withSystemTempFile "ema-tailwind-tmpfile" $ \fp h -> do
    liftIO $ do
      print fp
      print s
      hPut h (encodeUtf8 s) >> hClose h
    f fp
      `finally` removeFile fp

callTailwind :: (MonadIO m, MonadLogger m) => [String] -> m ()
callTailwind args = do
  logInfoN $ "Running Tailwind compiler with args: " <> show args
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
