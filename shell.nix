{ pkgs ? import <nixpkgs> {} }:
with pkgs;
mkShell {
  name = "maybe-extra-shell";
  buildInputs = with elmPackages;
    [ elm
      elm-test
      elm-verify-examples
      elm-format
    ];
}
