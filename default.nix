{ nixpkgs   ? import <nixpkgs> { }
, compiler  ? "default"
, profiling ? false
}:

let
  pkgs = nixpkgs;

  buildInputs = with pkgs; [
    # List extra dependencies here.
  ];

in
  pkgs.nix-hs.interactive ./mkweb.nix
    { inherit compiler profiling buildInputs; }
