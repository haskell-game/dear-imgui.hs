{
  description = "Haskell bindings for Dear ImGui.";
  inputs.haskellNix = {
    url = "github:input-output-hk/haskell.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.self.submodules = true;
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      haskellNix,
    }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
    flake-utils.lib.eachSystem supportedSystems (
      system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            dearImguiProject = final.haskell-nix.project' {
              src = final.haskell-nix.haskellLib.cleanSourceWith {
                name = "dear-imgui-src";
                src = ./.;
              };
              compiler-nix-name = "ghc9103";
              modules = [
                ({ pkgs, ... }: {
                  packages.dear-imgui.components.library.pkgconfig = pkgs.lib.optionals pkgs.stdenv.isLinux [
                    [ pkgs.libx11 ]
                  ];
                })
              ];
              shell.tools = {
                cabal = { };
                haskell-language-server = { };
              };
              shell.buildInputs =
                with final;
                [
                  # Make `haskell-language-server-wrapper` available (`haskell.nix` does not provide it)
                  # Copied from https://github.com/IntersectMBO/cardano-api/blob/master/flake.nix
                  (writeShellScriptBin "haskell-language-server-wrapper" ''exec haskell-language-server "$@"'')

                  nixfmt

                  pkg-config
                  SDL2
                  glew
                  glfw
                ]
                ++ lib.optionals stdenv.isLinux [
                  libGL
                  libx11
                  libxcursor
                  libxext
                  libxi
                  libxinerama
                  libxrandr
                  libxxf86vm
                ]
                ++ lib.optionals stdenv.isDarwin (
                  with darwin.apple_sdk.frameworks;
                  [
                    AGL
                    Cocoa
                    OpenGL
                    IOKit
                    Kernel
                    CoreVideo
                  ]
                  ++ [ darwin.CF ]
                );
              shell.withHoogle = true;
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.dearImguiProject.flake { };
      in
      flake
      // {
        packages.default = flake.packages."dear-imgui:lib:dear-imgui";
      }
    );

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
