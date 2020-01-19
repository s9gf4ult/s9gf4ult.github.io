{ sources ? import ./nix/sources.nix }:

let
  nixpkgs = import sources.nixpkgs {} ;
  blog = nixpkgs.pkgs.haskellPackages.callPackage ./default.nix {} ;

in blog
