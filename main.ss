(load-shared-object "raw-mode.so")

(define enable-raw-mode
  (foreign-procedure "enableRawMode" () void*))

(define disable-raw-mode
  (foreign-procedure "disableRawMode" () void*))

(define (term-clear)
  (for-each display '(#\esc #\[ 2 #\J)))

(define (term-hide-cursor)
  (for-each display '(#\esc #\[ #\? 2 5 #\l)))

(define (term-show-cursor)
  (for-each display '(#\esc #\[ #\? 2 5 #\h)))

(define (term-pin-cursor x y)
  (for-each display (list #\esc #\[ x #\; y #\H)))

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

(define term-cursor-move
  (lambda (x y)
    (let ([x-op (if (positive? x)
		  term-cursor-right
		  term-cursor-left)]
	  [y-op (if (positive? y)
		  term-cursor-down
		  term-cursor-up)])
      (begin
	(x-op (abs x))
	(y-op (abs y))))))

(define term-delete-n-char-cursor-right
  (lambda (n)
    (for-each display '(#\esc #\[ n #\P))))

(define CONTROL #\x1f)

(define char-and
  (lambda (c1 c2)
    (integer->char
      (bitwise-and
	(char->integer c1)
	(char->integer c2)))))

(define get-lines
  (lambda (port)
    (let loop ([lines '()]
	       [line (get-line port)])
      (if (eof-object? line)
	lines
	(loop (append lines (list line))
	      (get-line port))))))

(define display-line
  (lambda (line)
    (let ([cr-lf "\r\n"])
      (display (string-append line cr-lf)))))

(define init-file
  (lambda (file)
    (call-with-input-file
      file
      (lambda (port)
	(let ([lines (get-lines port)])
	  (set! *EDITING* "")
	  (set! *FILE* lines)
	  (for-each display-line
		    lines))))))

(define exit
  (lambda ()
    (set! *FILE* #f)))

(define add-char 
  (lambda (ch)
    (set! *EDITING*
	 (string-append *EDITING* (list->string (list ch))))
    (display ch)))

(define lines->string
  (lambda (lines)
    (fold-right
      (lambda (x a)
	(string-append x "\r\n" a))
      "" lines)))

(define save
  (lambda (file)
      (call-with-output-file
	file
	(lambda (port)
	  (set! *FILE* (append *FILE* (list *EDITING*)))
	  (set! *EDITING* "")
	  (put-string port (lines->string *FILE*)))
	'(truncate))))

(define delete-char
  (lambda ()
    (term-cursor-left 1)
    (display #\space)
    (term-cursor-left 1)
    ))

(define act
  (lambda (ch file)
    (case ch
      ([#\x13] (save file))
      ([#\delete] (delete-char))
      (else (add-char ch)))))
      

(define edit
  (lambda (file)
    (init-file file)
    (let loop ([ch (read-char)])
      (case ch
	([#\x11] (exit))
	(else
	  (begin
	    (act ch file)
	    (loop (read-char))))
	))))

(define main
  (lambda (file)
    (enable-raw-mode)
    (term-clear)
    (term-pin-cursor 1 1)

    (edit file)

    (term-pin-cursor 1 1)
    (term-clear)
    (disable-raw-mode)
    ((lambda () (display #\newline)))))
