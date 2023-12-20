(lang dune 3.12)

(name chip8)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Antonis Kalou <kalouantonis@protonmail.com>")

(maintainers "Antonis Kalou")

(license GPL-3.0-only)

; (documentation https://url/to/documentation)

(package
 (name chip8)
 (synopsis "A chip8 VM")
 (description "A chip8 virtual machine written in OCaml")
 (depends
  (ocaml (>= 5.0.0))
  dune
  (stdint (>= 0.7.2))
  (bigstringaf (>= 0.9.1))
  (tsdl (>= 1.0.0))))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
