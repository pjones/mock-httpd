{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghc ? "default"
}:

nix-hs {
  cabal = ./mock-httpd.cabal;
  compiler = ghc;

  overrides = lib: self: super: {
    relude =
      if super ? relude_0_6_0_0
      then super.relude_0_6_0_0
      else super.relude;
  };
}
