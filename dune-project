(lang dune 2.6)
(name hocl)
(version 1.2.1)
(source (github jserot/hocl))
(license MIT)
(authors "Jocelyn Sérot <jocelyn.serot@uca.fr>")
(maintainers "Jocelyn Sérot <jocelyn.serot@uca.fr>")
(bug_reports "jocelyn.serot@uca.fr")
(homepage "http://github.com/jserot/hocl")
(using menhir 2.0)
(wrapped_executables false)
(allow_approximate_merlin)

(package
  (name hocl)
  (synopsis "language for describing dataflow process networks and generating tool-specific descriptions.")
  (depends
    (ezxmlm (>= 1.1.0))
    (menhir (>= 20200624))
    (ocaml (>= 4.10))))

(generate_opam_files true)
