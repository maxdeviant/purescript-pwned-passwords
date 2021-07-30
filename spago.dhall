{ name = "pwned-passwords"
, license = "MIT"
, repository = "https://github.com/maxdeviant/purescript-pwned-passwords"
, dependencies =
  [ "aff"
  , "affjax"
  , "arrays"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "integers"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
