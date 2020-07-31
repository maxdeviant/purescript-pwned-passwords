{ name = "my-project"
, dependencies =
  [ "affjax", "console", "effect", "integers", "psci-support", "stringutils" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
