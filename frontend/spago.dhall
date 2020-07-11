{ name = "higaidar-frontend"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "foreign-object"
  , "halogen"
  , "math"
  , "monad-loops"
  , "nullable"
  , "ordered-collections"
  , "parsing"
  , "precise-datetime"
  , "psci-support"
  , "random"
  , "routing"
  , "uri"
  , "web-dom"
  , "web-events"
  , "web-file"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
