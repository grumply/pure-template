{ mkDerivation, base, haskell-src-exts, haskell-src-meta, pure-txt, pure-txt-interpolate, stdenv }:
mkDerivation {
  pname = "pure-template";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base haskell-src-exts haskell-src-meta pure-txt pure-txt-interpolate ];
  homepage = "github.com/grumply/pure-template";
  description = "String interpolating template macros";
  license = stdenv.lib.licenses.bsd3;
}
