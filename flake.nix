{
description = "Flake to make java available for Flix.";

inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";

outputs = inputs:
let
  system = "x86_64-darwin";
  pkgs = inputs.nixpkgs.legacyPackages.${system};
  # pkgs = inputs.nixpkgs
  jdk = pkgs.openjdk17;
in {
  devShell.${system} = pkgs.mkShell rec {
    name = "java-shell";
    buildInputs = with pkgs; [ jdk nodejs-18_x nodejs-18_x.pkgs.pnpm ];

    shellHook = ''
      export JAVA_HOME=${jdk}
      PATH="${jdk}/bin:$PATH"
      # flakes devShell will set CLASSPATH to an empty string which will
      # somehow get translated to CLASSPATH='null' in the version detection
      # of the vscode-flix plugin and caused this plugin to fail.
      unset CLASSPATH
    '';
  };
 };
}
