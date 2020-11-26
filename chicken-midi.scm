(module chicken-midi ()
  (import scheme)
  (import (chicken base))
  (import (chicken module))

  (import srfi-4)
  (import srfi-178)

  (export
   midi-open-file)

  (include "chicken-midi-impl.scm"))
