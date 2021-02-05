{ # Fetch haskell.nix and import its default.nix
haskellNix ? (import (import ./nix/sources.nix)."haskell.nix" { })

# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009

  # haskell.nix provides some arguments to be passed to nixpkgs, including some
  # patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, compiler-nix-name ? "ghc884"
}:
let
  pkgs = import nixpkgsSrc nixpkgsArgs;
in pkgs.haskell-nix.project {
  inherit compiler-nix-name;
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "dear-imgui";
    src = ./.;
  };
  modules = [ {
    # This library needs libXext to build, but doesn't explicitly state it in
    # its .cabal file.
    packages.bindings-GLFW.components.library.libs =
      pkgs.lib.mkForce (
        pkgs.lib.optionals   pkgs.stdenv.isDarwin  (with pkgs.darwin.apple_sdk.frameworks; [ AGL Cocoa OpenGL IOKit Kernel CoreVideo pkgs.darwin.CF ]) ++
        pkgs.lib.optionals (!pkgs.stdenv.isDarwin) (with pkgs.xorg; [ libXext libXi libXrandr libXxf86vm libXcursor libXinerama pkgs.libGL ])
      );

    # Depends on libX11 but doesn't state it in the .cabal file.
    packages.GLFW-b.components.library.libs =
      with pkgs.xorg;
      pkgs.lib.mkForce [ libX11 ];
  } ];
}
