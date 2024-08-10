(define string-delete!
  (lambda (str pos)
    (let ([len (string-length str)])
      (let ([str1 (substring str 0 (- pos 1))]
	    [str2 (substring str pos len)])
	(string-append str1 str2)))))

(define string-insert!
  (lambda (str pos ch)
    (let ([len (string-length str)])
      (let ([str1 (substring str 0 pos)]
	    [str2 (substring str pos len)])
	(string-append str1 (string ch) str2)
	))))

(define string->char
    (lambda (str)
      (if (> (string-length str) 1)
	  (char-name (string->symbol str))
	  (car (string->list str)))))

; only works for trim string
(define string-split
    (lambda (str ch)
      (let ([chars (string->list str)])
        (let loop ([result '()]
                   [char-list '()]
                   [rest chars])
          (if (null? rest)
	    (map
	      list->string
              (append result (list char-list)))
              (let ([next-ch (car rest)])
                (if (char=? ch next-ch)
                    (loop
                      (append result (list char-list))
                      '()
                      (cdr rest))
                    (loop
                      result
                      (append char-list (list next-ch))
                      (cdr rest)))))))))
