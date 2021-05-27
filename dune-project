(lang dune 2.8)
(name hotcaml)
(version 0.1)

(generate_opam_files true)

; (source (github mirage/ocaml-cohttp))

(license ISC)
(authors "Frédéric Bour")
(maintainers "frederic.bour@lakaban.net")

(package
 (name hotcaml)
 (synopsis "An OCaml interpreter that reload source files")
 (description "Interactive development by automatically reloading module dependencies"))
