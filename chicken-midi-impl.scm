;; For MIDI version 1.1, the chunk type must be:
(define midi-header-chunk-type (string->utf8 "MThd"))

(define midi-header-chunk-type-length (bytevector-length midi-header-chunk-type))

;; For MIDI version 1.1, a length of 6 is the only currently valid length of
;; the data portion of a MIDI header.
(define-constant midi-header-length-field #u8(0 0 0 6))

(define midi-header-length-field-length
  (bytevector-length midi-header-length-field))

(define midi-header-format-field-length 4)
(define midi-header-tracks-field-length 4)
(define midi-header-division-field-length 4)

(define midi-header-length
  (+
   midi-header-chunk-type-length
   midi-header-format-field-length
   midi-header-tracks-field-length
   midi-header-division-field-length))

;; For MIDI version 1.1, the chunk type and length of a MIDI header
;; are pre-defined, so we simply check to make sure they have their
;; expected values, and then return the only variable parts
;; of a MIDI header, which are: format, tracks, and division.
;;
;; We also return the port, which contains the remaining contents
;; of the MIDI file, in binary format.
(define (midi-read-header filename)
  (if (file-exists? filename)
      (if (< (file-size filename) midi-header-length)
          (error "midi-read-file: file too short to be a valid MIDI file")
          (let*
              ((port (open-binary-input-file filename))
               (chunk-type (read-bytevector midi-header-chunk-type-length     port))
               (len        (read-bytevector midi-header-length-field-length   port))
               (format     (read-bytevector midi-header-format-field-length   port))
               (tracks     (read-bytevector midi-header-tracks-field-length   port))
               (division   (read-bytevector midi-header-division-field-length port)))
            (if (equal? chunk-type midi-header-chunk-type)
                (if (equal? len midi-header-length-field)
                    (let ((format
                           (match format
                             (#u8(0 0 0 0) 0)
                             (#u8(0 0 0 1) 1)
                             (#u8(0 0 0 2) 2)
                             (else
                              (error
                               "midi-read-header: unexpected format" format)))))
                      (values format tracks division port))
                    (error "midi-read-header: unexpected length" len))
                (error "midi-read-header: unexpected chunk-type" chunk-type))))
      (error "midi-read-header: non-existant file" filename)))

(define (midi-read-file filename)
  (let ((format
         tracks
         division
         port
         (midi-read-header filename)))
    '()))
