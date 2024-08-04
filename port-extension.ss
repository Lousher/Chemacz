(define get-lines
    (lambda (port)
      (let loop ([result '()]
                 [line (get-line port)])
        (if (eof-object? line)
            result
            (loop
              (append
                result
                (list line))
              (get-line port))))))
