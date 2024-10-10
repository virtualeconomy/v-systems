{ pkgs, lib, config, inputs, ... }:

{
	languages.java.jdk.package = pkgs.jdk21;
	languages.scala.package = pkgs.scala;
	languages.scala.enable = true;
	languages.scala.sbt.package = pkgs.sbt;
	languages.scala.sbt.enable = true;

  packages = [ pkgs.curl pkgs.git ];

	processes = {
		sbt-package.exec = "sbt packageAll; exit";
	};

  enterShell = ''
	echo "SBT environment"
  '';
}
