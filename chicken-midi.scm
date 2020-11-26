(module chicken-midi ()
  (import r7rs)

  (import srfi-4)
  (import srfi-178)

  (export
   midi-read-file)

  (include "chicken-midi-impl.scm"))
