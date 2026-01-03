(lang dune 3.20)

(name shibboleth)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)
(using menhir 3.0)
(package
 (name shibboleth)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  (alcotest :with-test)
  ppxlib
  cmdliner
  menhir 
  ppx_import 
  ppx_deriving)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
