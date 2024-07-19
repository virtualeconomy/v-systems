{
  description = "A Scala project with SBT";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        sbt = pkgs.sbt;
        jdk = pkgs.jdk21;
        scala = pkgs.scala;
      in {
        devShell = pkgs.mkShell {
          buildInputs = [ sbt jdk scala ];
          shellHook = ''
            echo "SBT and Scala environment ready!"
          '';
        };

        packages.default = pkgs.stdenv.mkDerivation {
          name = "flake-vsys-build-with-sbt";
          src = ./.;
          buildInputs = [ sbt jdk scala ];
          buildPhase = ''
            echo "Building with SBT..."

            sed -i 's/scalaVersion in ThisBuild := "2.12.6"/scalaVersion in ThisBuild := "2.12.18"/' build.sbt
            sed -i 's/sbt.version=1.1.1/sbt.version=1.9.9/' project/build.properties

            export HOME=~

            sbt packageAll
          '';
          installPhase = ''
            mkdir -p $out
            cp -r target $out
          '';
        };
      }
    );
}
