with import <nixpkgs> { };
stdenv.mkDerivation {
  name = "fakedata";
  buildInputs = [
    ghc
    zlib
  ];
}
