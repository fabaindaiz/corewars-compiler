(lang dune 3.18)

(name corewars-compiler)

(generate_opam_files true)

(source
 (github fabaindaiz/corewars-compiler))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name corewars-compiler)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  alcotest
  containers
 )
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html