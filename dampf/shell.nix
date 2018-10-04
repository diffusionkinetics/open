{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, bytestring
      , containers, directory, exceptions, filepath, free, lens, mtl
      , optparse-applicative, optparse-generic, postgresql-simple, pretty
      , process, random, regex-compat, regex-posix, scientific, shelly
      , split, stdenv, text, time, transformers, typed-process, unix
      , unordered-containers, vector, wreq, yaml
      }:
      mkDerivation {
        pname = "dampf";
        version = "0.1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson attoparsec base bytestring containers directory exceptions
          filepath free lens mtl postgresql-simple pretty process random
          regex-compat regex-posix scientific shelly split text time
          transformers typed-process unix unordered-containers vector wreq
          yaml
        ];
        executableHaskellDepends = [
          base lens optparse-applicative optparse-generic text
        ];
        homepage = "https://github.com/diffusionkinetics/open/dampf";
        description = "Declarative DevOps for busy developers";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
