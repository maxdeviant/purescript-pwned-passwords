module PwnedPasswords
  ( PasswordStatus(..)
  , Error
  , printError
  , pwned
  ) where

import Prelude
import Affjax as Ax
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (ExceptT(..), except, runExceptT)
import Data.Array (find, mapMaybe)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, toUpper)
import Data.String.CodeUnits (slice)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..), fst)
import Effect.Aff (Aff)
import Sha1 (sha1)

-- | The status of a password after checking it against [Pwned Passwords](https://haveibeenpwned.com/Passwords).
data PasswordStatus
  -- | The password was not found in the Pwned Passwords index. This does
  -- | not necessarily mean it is a *good* password, just that it has not
  -- | appeared in any data breaches that Pwned Passwords has in its index.
  = NotFound
  -- | The password appears in a data breach indexed by Pwned Passwords. The
  -- | number indicates the number of times the password has been seen. This
  -- | password should never be used!
  | Pwned Int

instance showPasswordStatus :: Show PasswordStatus where
  show NotFound = "NotFound"
  show (Pwned count) = "Pwned " <> show count

data Error
  = AffjaxError Ax.Error
  | HashError

printError :: Error -> String
printError (AffjaxError err) = Ax.printError err

printError HashError = "There was a problem hashing the password."

-- | Checks the specified password against [Pwned Passwords](https://haveibeenpwned.com/Passwords)
-- | to determine whether it is safe for use.
pwned :: String -> Aff (Either Error PasswordStatus)
pwned password =
  runExceptT do
    hashPrefix <- except $ note HashError $ slice 0 5 hash
    hashSuffix <- except $ map mkHashSuffix $ note HashError $ slice 5 40 hash
    res <-
      ExceptT
        $ map (lmap AffjaxError)
        $ Ax.get ResponseFormat.string
        $ "https://api.pwnedpasswords.com/range/"
        <> hashPrefix
    pure
      $ res.body
      # lines
      # mapMaybe parseEntry
      # find (fst >>> eq hashSuffix)
      # case _ of
          Just (Tuple _ occurrences) -> Pwned occurrences
          Nothing -> NotFound
  where
  hash = sha1 password

data HashSuffix
  = HashSuffix String

mkHashSuffix :: String -> HashSuffix
mkHashSuffix = HashSuffix <<< toUpper

instance hashSuffixShow :: Show HashSuffix where
  show (HashSuffix suffix) = "HashSuffix " <> suffix

instance hashSuffixEq :: Eq HashSuffix where
  eq (HashSuffix a) (HashSuffix b) = a `eq` b

parseEntry :: String -> Maybe (Tuple HashSuffix Int)
parseEntry line = case split (Pattern ":") line of
  [ suffix, occurrences ] ->
    occurrences
      # Int.fromString
      # map (Tuple $ mkHashSuffix suffix)
  _ -> Nothing
