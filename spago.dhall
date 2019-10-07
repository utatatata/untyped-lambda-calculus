{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "arrays"
    , "console"
    , "effect"
    , "foldable-traversable"
    , "psci-support"
    , "spec"
    , "tuples"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
