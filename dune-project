(lang dune 3.20)

(name shibboleth)

(generate_opam_files true)

(source
 (github wizard7377/sml-ocaml-converter))

(authors "Asher Frost <wizardishlike@gmail.com>")

(maintainers "Asher Frost <wizardishlike@gmail.com>")

(license BSD-2-Clause)

(documentation https://url/to/documentation)
(using menhir 3.0)
(package
 (name shibboleth)
 (synopsis "A converter between SML and OCaml")
 (description "A tool that converts Standard ML code to OCaml code and vice versa, facilitating interoperability between the two languages.")
 (depends
  ocaml
  (alcotest :with-test)
  ppxlib
  cmdliner
  menhir 
  ppx_import 
  ppx_deriving
  ocamlformat
  odoc 
  menhir 
  re 
  )
 (tags
  ("ocaml" "sml")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
