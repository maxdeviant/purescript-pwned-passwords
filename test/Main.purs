module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Partial.Unsafe (unsafePartial)
import PwnedPasswords (printError, pwned)

main :: Effect Unit
main = do
  launchAff_ $ do
    result <- unsafePartial $ pwned "correct horse battery staple"
    case result of
      Left err -> log $ printError err
      Right status -> log $ show status

