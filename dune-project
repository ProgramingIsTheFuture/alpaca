(lang dune 3.10)

(name alpaca)

(generate_opam_files true)

(source
 (github ProgramingIsTheFuture/alpaca))

(authors "ProgramingIsTheFuture")

(maintainers "ProgramingIsTheFuture")

(package
 (name alpaca)
 (synopsis "Http library")
 (description "Http library")
 (depends ocaml dune eio_main eio))

(package
 (name test_alpaca)
 (allow_empty)
 (synopsis "Test http library")
 (description "Test http library")
 (depends ocaml dune alpaca))
