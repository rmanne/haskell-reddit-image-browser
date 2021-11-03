{
  description = "haskell-reddit-image-browser's description";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      dependencyOverlays = [
        (self: super: {
          # Overrides for ffmpeg-light
          libavutil = self.ffmpeg;
          libavformat = self.ffmpeg;
          libavcodec = self.ffmpeg;
          libswscale = self.ffmpeg;
          libavdevice = self.ffmpeg;
          libswresample = self.ffmpeg;
        })
      ];
      overlays = dependencyOverlays ++ [ haskellNix.overlay
        (final: prev: {
          haskell-reddit-image-browserProject =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                hlint = {};
                haskell-language-server = {};
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [ nixpkgs-fmt ];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.haskell-reddit-image-browserProject.flake { };
    in flake // { defaultPackage = flake.packages."haskell-reddit-image-browser:exe:rib"; });
}
