(define-constant bits-per-byte 8)

;; For MIDI version 1.1, the chunk type must be:
(define-constant midi-header-chunk-type "MThd")
(define midi-header-chunk-type-length-in-bytes
  (string-length midi-header-chunk-type))
(define midi-header-chunk-type-length-in-bits
  (*
   midi-header-chunk-type-length-in-bytes
   bits-per-byte))

;; For MIDI version 1.1, a length of 6 is the only currently valid length of
;; the data portion of a MIDI header.
(define-constant midi-header-length-field #u8(0 0 0 6))

(define midi-header-length-field-length-in-bytes
  (bytevector-length midi-header-length-field))

(define midi-header-length-field-length-in-bits
  (* midi-header-length-field-length-in-bytes
     bits-per-byte))

(define midi-header-format-field-length-in-bytes   2)
(define midi-header-tracks-field-length-in-bytes   2)
(define midi-header-division-field-length-in-bytes 2)

(define midi-header-format-field-length-in-bits
  (* midi-header-format-field-length-in-bytes
     bits-per-byte))

(define midi-header-tracks-field-length-in-bits
  (* midi-header-tracks-field-length-in-bytes
     bits-per-byte))

(define midi-header-division-field-length-in-bits
  (* midi-header-division-field-length-in-bytes
     bits-per-byte))

(define midi-header-length-in-bytes
  (+
   midi-header-chunk-type-length-in-bytes
   midi-header-format-field-length-in-bytes
   midi-header-tracks-field-length-in-bytes
   midi-header-division-field-length-in-bytes))

(define (midi-read-file-as-bytevector filename)
  (if (file-exists? filename)
      (let ((size (file-size filename)))
        (if (< size midi-header-length-in-bytes)
            (error "midi-read-file: file too short to be a valid MIDI file")
            (let* ((port (open-binary-input-file filename))
                   (file-as-bytevector (read-bytevector size port)))
              file-as-bytevector)))
      (error "midi-read-header: non-existant file" filename)))

;; For MIDI version 1.1, the chunk type and length of a MIDI header
;; are pre-defined, so we simply check to make sure they have their
;; expected values, and then return the only variable parts
;; of a MIDI header, which are: format, tracks, and division.
;;
;; We also return the rest of the file as a bitstring
(define (midi-read-header file-as-bytevector)
  (bitmatch file-as-bytevector
            (((#x4d546864 midi-header-chunk-type-length-in-bits)
              (6 midi-header-length-field-length-in-bits)
              (format midi-header-format-field-length-in-bits)
              (tracks midi-header-tracks-field-length-in-bits)
              (division midi-header-division-field-length-in-bits)
              (rest bitstring))
             (list format tracks division rest))
            (else (error "midi-read-header: invalid header"))))

(define (midi-read-file filename)
  (let* ((file-as-bytevector (midi-read-file-as-bytevector filename))
         (result
          (midi-read-header file-as-bytevector)))
    (match result
      ((format tracks division rest)
       (let ((division (midi-parse-division division)))
         (midi-validate-format format)
         (printf "format: '~S'~%" format)
         (printf "tracks: '~S'~%" tracks)
         (printf "division: '~S'~%" division))))))

;; For MIDI 1.1, the only valid formats are 0, 1, and 2
(define (midi-validate-format format)
  (match format
    (0 0)
    (1 1)
    (2 2)
    (else (error "midi-read-header: unexpected format"))))

(define (midi-parse-division division)
  (let ((ticks-per-quarter-note-field-length-in-bits 15)
        (frames-per-sec-field-length-in-bits 7)
        (ticks-per-frame-field-length-in-bits 8))
    (bitmatch
     ;; division comes in as an integer, which bitmatch won't work with,
     ;; so we need to turn it back in to a bitstring:
     (bitconstruct (division midi-header-division-field-length-in-bits))
     ;; Now we can match against it:
     (((0 1)
       (ticks-per-quarter-note ticks-per-quarter-note-field-length-in-bits))
      (list
       'ticks-per-quarter-note ticks-per-quarter-note))
     (((1 1)
       (frames-per-sec  frames-per-sec-field-length-in-bits)
       (ticks-per-frame ticks-per-frame-field-length-in-bits))
      (list
       'frames-per-sec
       (- 0 frames-per-sec)
       'ticks-per-frame
       ticks-per-frame))
     (else
      (print "midi-read-header: invalid division")))))
