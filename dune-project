(lang dune 3.4)

(name toy_compiler)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name toy_compiler)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
 (ocaml (>= 4.10))
  dune sedlex menhir menhirLib llvm cmdliner alcotest ppx_expect ppx_deriving)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
