;; Copyright (C) 2020 - Sergey Goldgaber
;;
;; This file is part of chicken-midi.
;; chicken-midi is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; chicken-midi is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with chicken-midi.  If not, see <http://www.gnu.org/licenses/>.

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
