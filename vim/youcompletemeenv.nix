let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in rec {
  youCompleteMeEnv = stdenv.mkDerivation rec {
    name = "youcompleteme-env";
    version = "0.1.0";
    buildInputs = with pkgs; [ cmake python ];
  };
}

