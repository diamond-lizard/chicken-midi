(module chicken-midi ()
  (import r7rs)
  ; r7rs libraries:
  (import (scheme file))
  (import (scheme write))
  (import (chicken base))
  (import (chicken format))
  (import (chicken file posix))

  (import bitstring)
  (import matchable)
  (import srfi-71)

  (export
   midi-read-file)

  (include "chicken-midi-impl.scm"))
