let
 pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  # build tools
  buildInputs = with pkgs; [cabal-install ghc];
}
