(module chicken-midi ()
  (import r7rs)
  ; r7rs libraries:
  (import (scheme file))
  (import (scheme write))
  (import (chicken file posix))

  (import matchable)
  (import srfi-71)
  (import srfi-178)

  (export
   midi-read-file)

  (include "chicken-midi-impl.scm"))
