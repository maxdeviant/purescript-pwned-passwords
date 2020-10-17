{ name = "pwned-passwords"
, license = "MIT"
, repository = "https://github.com/maxdeviant/purescript-pwned-passwords"
, dependencies =
  [ "affjax"
  , "console"
  , "effect"
  , "integers"
  , "psci-support"
  , "stringutils"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
