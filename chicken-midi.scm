(module chicken-midi ()
  (import r7rs)
  ; r7rs libraries:
  (import (scheme file))

  (import srfi-4)
  (import srfi-178)

  (export
   midi-read-file)

  (include "chicken-midi-impl.scm"))
