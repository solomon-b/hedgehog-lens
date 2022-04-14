{ pkgs ? import <unstable> {} }:
let
  editorTooling = [
    pkgs.cabal-install
    pkgs.ghcid
    pkgs.haskell.packages.ghc8107.haskell-language-server
  ];

  buildDeps = [ pkgs.haskell.compiler.ghc8107 ];
in
pkgs.mkShell {
  buildInputs = editorTooling ++ buildDeps;
}
