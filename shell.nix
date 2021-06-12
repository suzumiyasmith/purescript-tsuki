{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    nodePackages.pnpm spago git nodejs pscid purescript purescript-psa closurecompiler esbuild
  ];
}
