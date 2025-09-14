{
  entries = [
    {
      description = "Map with an index",
      signature = "(num -> a -> b) -> [a] -> [b]",

      hs = "flip zipWith [0..]",
      hsUrl = "https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:zipWith",

      purs = "mapWithIndex",
      pursUrl = "https://pursuit.purescript.org/packages/purescript-arrays/7.3.0/docs/Data.Array#v:mapWithIndex",

      elm = "indexedMap",
      elmUrl = "https://package.elm-lang.org/packages/elm/core/latest/Array#indexedMap",

      nix = "imap0",
      nixUrl = "https://noogle.dev/f/lib/imap0",

      typst = "a.enumerate().map((i, v) => …)",
      typstUrl = "https://github.com/typst/typst/discussions/3741",

      ocaml = "mapi",
      ocamlUrl = "https://ocaml.org/manual/5.3/api/List.html#VALmapi",
    }
  ]
}
