{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Test.Hspec (describe, hspec, it, shouldBe)
import Web.Tailwind
import Data.Default (Default(def))
import NeatInterpolation (text)
import Optics.Core ((.~))

strDefaultConfig :: Text
strDefaultConfig = [text|
  module.exports = {
      content: [],
      plugins: [
          require('@tailwindcss/typography'),
          require('@tailwindcss/forms'),
          require('@tailwindcss/line-clamp'),
          require('@tailwindcss/aspect-ratio')
      ]
  }
  |]

strConfigNoPlugins :: Text
strConfigNoPlugins = [text|
  module.exports = {
      content: [],
  }
  |]

strConfigSomePlugins :: Text
strConfigSomePlugins = [text|
  module.exports = {
      content: [],
      plugins: [
          require('@tailwindcss/aspect-ratio'),
          require('@tailwindcss/forms'),
          require('@tailwindcss/typography')
      ]
  }
  |]

main :: IO ()
main = hspec $ do
  describe "TailwindConfig" do
    it "matches the default .js file content" $
      show (def :: TailwindConfig) `shouldBe` strDefaultConfig
    it "matches the .js file content w/o plugins" $
      show (def & tailwindConfigPlugins .~ []) `shouldBe` strConfigNoPlugins
    it "matches the .js file content w/ some plugins" $
      show (def & tailwindConfigPlugins .~ [AspectRatio, Forms, Typography])
        `shouldBe` strConfigSomePlugins
