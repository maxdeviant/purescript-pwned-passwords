module PwnedPasswords
  ( PasswordStatus(..)
  , pwned
  ) where

import Prelude
import Affjax (Error)
import Affjax as Ax
import Affjax.ResponseFormat as ResponseFormat
import Data.Array (find, mapMaybe)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust)
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

data HashSuffix
  = HashSuffix String

mkHashSuffix :: String -> HashSuffix
mkHashSuffix = HashSuffix <<< toUpper

instance hashSuffixShow :: Show HashSuffix where
  show (HashSuffix suffix) = "HashSuffix " <> suffix

instance hashSuffixEq :: Eq HashSuffix where
  eq (HashSuffix a) (HashSuffix b) = a `eq` b

-- | Checks the specified password against [Pwned Passwords](https://haveibeenpwned.com/Passwords)
-- | to determine whether it is safe for use.
pwned :: Partial => String -> Aff (Either Error PasswordStatus)
pwned password = do
  result <- Ax.get ResponseFormat.string url
  pure
    $ case result of
        Left err -> Left err
        Right res ->
          res.body
            # lines
            # mapMaybe parseEntry
            # find (fst >>> eq hashSuffix)
            # case _ of
                Just (Tuple _ occurrences) -> Right $ Pwned occurrences
                Nothing -> Right $ NotFound
  where
  hash = sha1 password

  hashPrefix = fromJust <<< slice 0 5 $ hash

  hashSuffix = mkHashSuffix $ fromJust <<< slice 5 40 $ hash

  url = "https://api.pwnedpasswords.com/range/" <> hashPrefix

parseEntry :: String -> Maybe (Tuple HashSuffix Int)
parseEntry line = case split (Pattern ":") line of
  [ suffix, occurrences ] ->
    occurrences
      # Int.fromString
      # map (Tuple $ mkHashSuffix suffix)
  _ -> Nothing
