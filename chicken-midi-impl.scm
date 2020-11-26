;; For MIDI version 1.1, a length of 6 is the only currently valid length of
;; the data portion of a MIDI header.
(define-constant expected-len #u8(0 0 0 6))

;; For MIDI version 1.1, the chunk type and length of a MIDI header
;; are pre-defined, so we simply check to make sure they have their
;; expected values, and then return the only variable parts
;; of a MIDI header, which are: format, tracks, and division.
;;
;; We also return the port, which contains the remaining contents
;; of the MIDI file, in binary format.
(define (midi-read-header filename)
  (if (file-exists? filename)
      (if (< (file-size filename) 14)
          (error "midi-read-file: file too short to be a valid MIDI file")
          (let* ((port (open-binary-input-file filename))
                 (chunk-type (read-bytevector 4 port))
                 (expected-chunk-type (string->utf8 "MThd"))
                 (len (read-bytevector 4 port))
                 (format (read-bytevector 4 port))
                 (tracks (read-bytevector 4 port))
                 (division (read-bytevector 4 port)))
            (if (equal? chunk-type expected-chunk-type)
                (if (equal? len expected-len)
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
