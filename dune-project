(lang dune 2.5)

(generate_opam_files true)

(name ics)

(authors "SRI International Computer Science Laboratory")

(maintainers "Josh Berdine <josh@berdine.com>")

(license MIT)

(source
 (github SRI-CSL/ICS))

(package
 (name ics)
 (synopsis "Integrated Canonizer and Solver")
 (description
   "Decision procedure for the combination of theories based on Shostak's procedure.")
 (depends
  (ocaml
   (>= 4.07))
  dune-build-info))
