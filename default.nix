{ # Fetch the latest haskell.nix and import its default.nix
  haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") {}

# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003

# haskell.nix provides some arguments to be passed to nixpkgs, including some
# patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, compiler-nix-name ? "ghc884"
}: 
let
  # Define custom nixpkgs overlay
  overlays = haskellNix.overlays ++ [
    (self: super: {
        sdl2 = super.haskellPackages.sdl2;
        GL = super.libGL;
        llvm = super.llvm;
    })
  ];
  pkgs = import nixpkgsSrc (nixpkgsArgs // { inherit overlays; });
in pkgs.haskell-nix.project { inherit compiler-nix-name;
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "dear-imgui";
    src = ./.;
  };
  
}