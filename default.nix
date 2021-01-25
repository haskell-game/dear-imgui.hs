{ # Fetch the latest haskell.nix and import its default.nix
haskellNix ? (import (import ./nix/sources.nix)."haskell.nix" { })

# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009

  # haskell.nix provides some arguments to be passed to nixpkgs, including some
  # patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs, compiler-nix-name ? "ghc884" }:
let
  pkgs = import nixpkgsSrc nixpkgsArgs;
in pkgs.haskell-nix.project {
  inherit compiler-nix-name;
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "dear-imgui";
    src = ./.;
  };
}