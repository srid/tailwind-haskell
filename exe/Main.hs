{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens.Operators
import Control.Monad.Logger.CallStack (runStdoutLoggingT)
import Data.Default (Default (def))
import Main.Utf8 (withUtf8)
import System.Environment (getArgs)
import Web.Tailwind

main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  withUtf8 $ do
    args <- getArgs
    print args
    runStdoutLoggingT $
      runTailwind $
        def & tailwindConfig . tailwindConfigContent .~ args
