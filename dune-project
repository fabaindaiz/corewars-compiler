(lang dune 3.10)

(name corewars_compiler)

(generate_opam_files true)

(source
 (github fabaindaiz/corewars-compiler))

(authors "fabaindaiz")

(maintainers "fabaindaiz")

(license GPL-3.0)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name corewars_compiler)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
 (ocaml (>= 4.10))
  dune sedlex menhir menhirLib cmdliner alcotest ppx_expect ppx_deriving)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
