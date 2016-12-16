with import <nixpkgs> {}; {
  cannyFreeRadicalEnv = stdenv.mkDerivation rec {
    name = "haskell-7.10.3";
    version = "0.1";
    src = ./.;
    buildInputs = [
        stdenv
        stack
        gmp
        cabal-install
        ghc
    ];
  };
}
