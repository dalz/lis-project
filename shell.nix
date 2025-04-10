{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; with ocamlPackages; [
    ocaml

    opam
    dune_3

    utop
    merlin
    ocamlformat
    ocaml-lsp
  ];

  shellHook = ''
    eval $(opam env --switch=.)
  '';

  MERLIN_SITE_LISP = "${pkgs.ocamlPackages.merlin}/share/emacs/site-lisp";
}
