{ mkDerivation, aeson, base, bytestring, containers, directory
, edify, filepath, hakyll, hjsmin, stdenv, text, time, yaml
}:
mkDerivation {
  pname = "mkweb";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory edify filepath hakyll
    hjsmin text time yaml
  ];
  executableHaskellDepends = [ base hakyll ];
  homepage = "http://github.com/pjones/mkweb";
  description = "Generic site generator using Hakyll";
  license = stdenv.lib.licenses.bsd2;
}
