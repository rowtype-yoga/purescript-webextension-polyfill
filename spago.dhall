{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "webextension-polyfill"
, repository = "https://github.com/rowtype-yoga/purescript-webextension-polyfill.git"
, license = "MIT"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "foreign-object"
  , "maybe"
  , "newtype"
  , "prelude"
  , "psci-support"
  , "yoga-json"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}