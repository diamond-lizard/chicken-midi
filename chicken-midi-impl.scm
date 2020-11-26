(define (midi-read-file filename)
  (let* ((port (open-binary-input-file filename))
         (file (read-u8vector #f port)))
    (display file)))
