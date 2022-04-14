{
  description = "Hedgehog properties for lens laws";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.cabal-install
            pkgs.ghcid
            pkgs.haskell.compiler.ghc8107
            pkgs.haskell.packages.ghc8107.haskell-language-server
            pkgs.nixpkgs-fmt
          ] ++ [
            # TODO: Bump this when a newer version of nixpkgs permits it.
            pkgs.haskellPackages.cabal-fmt
            pkgs.haskellPackages.ormolu
          ];
        };
      }
    );
}
