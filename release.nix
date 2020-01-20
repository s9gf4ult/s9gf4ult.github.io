{ sources ? import ./nix/sources.nix }:

let
  nixpkgs = import sources.nixpkgs {} ;
  hpkgs = nixpkgs.pkgs.haskellPackages.override {
    overrides = self: super: {
      blog = self.callPackage ./default.nix {} ;
    };
  };

in hpkgs.blog
