(define (midi-read-from-port port)
  (let ((file (read-u8vector #f port)))
    (print file)))

(define (midi-open-file filename mode)
  (call-with-input-file filename
    (lambda (port)
      (midi-read-from-port port))))
