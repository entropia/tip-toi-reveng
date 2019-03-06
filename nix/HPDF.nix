{ mkDerivation, array, base, base64-bytestring, binary, bytestring
, containers, errors, filepath, HTF, mtl, network-uri, parsec
, random, stdenv, text, vector, zlib, fetchgit
}:
mkDerivation {
  pname = "HPDF";
  version = "1.4.10";
  src = fetchgit {
    url = "https://github.com/nomeata/HPDF";
    rev = "1062cf0378c6bff384380101821e6744d41d0484";
    sha256 = "0jjam8s0sl9abgd1a9l6d0ilmzp0l34kvw24548m10r3fg7jc6kd";
  };
  isLibrary = true;
  isExecutable = false;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array base base64-bytestring binary bytestring containers errors
    filepath mtl network-uri parsec random text vector zlib
  ];
  executableHaskellDepends = [
    base filepath network-uri random text vector
  ];
  testHaskellDepends = [ base HTF ];
  homepage = "http://www.alpheccar.org";
  description = "Generation of PDF documents";
  license = stdenv.lib.licenses.bsd3;
}
