(lang dune 2.9)

(name actorsocaml)

(generate_opam_files true)

(authors "Martin Andrieux")

(maintainers "Martin Andrieux")

(package
 (name actorsocaml)
 (synopsis "An actor library for OCaml 5")
 (description "An actor library for OCaml 5")
 (depends ocaml dune))

(package
 (name actorsocamlppx)
 (synopsis "A ppx rewritter for ActorsOcaml")
 (description "A ppx rewritter for ActorsOcaml")
 (depends ocaml ppxlib dune))

(package
 (name name_resolver)
 (synopsis "A ppx which resolve names")
 (description "A ppx which resolve names")
 (depends ocaml ppxlib dune))

 ; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
