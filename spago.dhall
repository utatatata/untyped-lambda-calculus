{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "arrays"
    , "console"
    , "css"
    , "effect"
    , "foldable-traversable"
    , "halogen"
    , "halogen-css"
    , "lists"
    , "parsing"
    , "psci-support"
    , "spec"
    , "tuples"
    , "web-clipboard"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
