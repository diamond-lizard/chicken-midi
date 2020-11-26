(define (midi-read-header filename)
  (if (file-exists? filename)
      (if (< (file-size filename) 14)
          (error "midi-read-file: file too short to be a valid MIDI file")
          (let* ((port (open-binary-input-file filename))
                 (chunk-type (read-bytevector 4 port))
                 (expected-chunk-type (string->utf8 "MThd"))
                 (len (read-bytevector 4 port))
                 (expected-len #u8(0 0 0 6))
                 (format (read-bytevector 4 port))
                 (tracks (read-bytevector 4 port))
                 (division (read-bytevector 4 port)))
            (if (equal? chunk-type expected-chunk-type)
                (if (equal? len expected-len)
                    (values format tracks division port)
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
