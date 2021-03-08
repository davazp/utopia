{ sources ? import ./nix/sources.nix, pkgs ? import sources.nixpkgs { } }:

with pkgs;

buildEnv {
  name = "davazp";
  paths = [ emacs htop tree jq coreutils niv ];
}
