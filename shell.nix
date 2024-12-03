{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;
#  local-hpkgs = localpkgs.haskellPackages;
 
in
pkgs.mkShell {
  buildInputs = with pkgs.haskellPackages; [ 
    ghc 
    ghcid 
    stack
    cabal-install
    implicit-hie
    haskell-language-server
    ];
}
