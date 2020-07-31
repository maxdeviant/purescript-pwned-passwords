{ name = "my-project"
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
