{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, data-default
      , dom-selector, HandsomeSoup, html-conduit, http-conduit
      , http-types, hxt, hxt-tagsoup, MissingH, mtl, network, old-locale
      , parsec, postgresql-simple, split, stdenv, stm, tagsoup
      , template-haskell, text, time, turtle, xml-conduit
      }:
      mkDerivation {
        pname = "train-delays";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring containers data-default dom-selector HandsomeSoup
          html-conduit http-conduit http-types hxt hxt-tagsoup MissingH mtl
          network old-locale parsec postgresql-simple split stm tagsoup
          template-haskell text time turtle xml-conduit
        ];
        description = "Crawls the infopasazer DB and estimates train delays";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
