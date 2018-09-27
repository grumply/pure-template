{ mkDerivation, base, haskell-src-meta, interpolate, stdenv }:
mkDerivation {
  pname = "pure-template";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ haskell-src-meta interpolate ];
  homepage = "github.com/grumply/pure-template";
  description = "String interpolating template macros";
  license = stdenv.lib.licenses.bsd3;
}
