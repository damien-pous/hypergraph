(lang dune 3.14)

(name hypergraphs)

(generate_opam_files true)

(source
 (github damien-pous/hypergraphs))

(authors "Damien Pous")

(maintainers "Damien Pous")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.0)

(package
 (name hypergraphs)
 (synopsis "visualize and edit hypergraphs")
 (description "visualize and edit sourced labelled hypergraphs")
 (depends ocaml dune cairo2-gtk vg)
 (tags
   ("hypergraph" "treewidth")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
