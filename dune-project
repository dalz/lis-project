(lang dune 3.18)
(using menhir 3.0)

(name lis-project)

(generate_opam_files true)

(source
 (github dalz/lis-project))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name lis-project)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml base ppx_deriving menhir)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
