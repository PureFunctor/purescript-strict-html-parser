{ name = "strict-html-parser"
, dependencies =
  [ "arrays"
  , "control"
  , "either"
  , "lists"
  , "prelude"
  , "psci-support"
  , "string-parsers"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/PureFunctor/strict-html-parser.git"
}
