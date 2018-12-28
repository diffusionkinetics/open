{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, hpack, servant-server
      , stdenv, text, wai-cli
      }:
      mkDerivation {
        pname = "formdata";
        version = "0.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          base bytestring servant-server text wai-cli
        ];
        preConfigure = "hpack";
        license = stdenv.lib.licenses.unfree;
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

  image = nixpkgs.dockerTools.buildImage {
    name = "formdata";
    contents = nixpkgs.haskell.lib.justStaticExecutables drv;
    config = {
      Cmd = [ "/bin/formdata" ];
      ExposedPorts =  { "3000" = {}; };
      Volumes = {
        "/data" = {};
      };
    };
  };

in image
