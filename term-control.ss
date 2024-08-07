(define (term-display-line content)
  (term-display (string-append content "\r\n")))

(define (term-display item)
  (display item))

(define (term-display-seq seq)
  (for-each
    display
    (ch-seq-chars seq)))

(define (term-clear)
  (display "\x1B;[2J"))

(define (term-hide-cursor)
  (display "\x1B;[?25l"))

(define (term-show-cursor)
  (display "\x1B;[?25h"))

(define (term-pin-cursor x y)
  (for-each display (list #\esc #\[ x #\; y #\H)))

(define (term-get-cursor)
  (call-with-port
    (current-input-port)
    (lambda (port)
      (display "\x1B;[6n")
      (let loop ([ch (read-char port)]
		 [ch-li '()])
	(if (char=? ch #\R)
	  (let ([li (list-split (list-tail ch-li 2     ) #\;)])
	    (cons (string->number
		    (list->string
		      (car li)))
		  (string->number
		    (list->string
		      (cdr li)))))
	  (loop (read-char port)
		(append ch-li (list ch))))))))

(define term-clear-line-cursor-left
  (lambda ()
    (for-each display '(#\esc #\[ 1 #\K))))

(define term-clear-line-cursor-right
  (lambda ()
    (for-each display '(#\esc #\[ 0 #\K))))

(define term-cursor-up
  (lambda (n)
    (for-each display (list #\esc #\[ n #\A))))

(define term-cursor-down
  (lambda (n)
    (for-each display (list #\esc #\[ n #\B))))

(define term-cursor-right
  (lambda (n)
    (for-each display (list #\esc #\[ n #\C))))

(define term-cursor-left
  (lambda (n)
    (for-each display (list #\esc #\[ n #\D))))

(define term-delete-n-char-cursor-right
  (lambda (n)
    (for-each display (list #\esc #\[ n #\P))))
