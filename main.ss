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

;---------------------------------

(define *EXIT*)

; initially buffer are raw line strings
(define init-buffer
  (lambda (file)
    (call-with-input-file
      file
      (lambda (port)
	(let ([buffer (get-string-all port)])
	  (if (eof-object? buffer) "" buffer))))))

(define-record-type change
  (fields value))

(define @get-change-from-user
  (lambda ()
    (let ([ch (read-char)])
      (case ch
	([#\x11]
	 (begin
	   (set! *EXIT* #t)
	   (make-change "")))
	(else
	  (make-change (list->string (list ch))))))))

(define file->string
  (lambda (file)
    (call-with-input-file
      file
      get-string-all)))

(define @loop-change
  (lambda (buffer)
      (let loop
	([preview (display buffer)]
	 [changes '()])
	(if *EXIT*
	  (values buffer changes)
	  (let ([change (@get-change-from-user)])
	    (loop (display (change-value change))
		(append
		  changes
		  (list change))))))))

(define save-file
  (lambda (buffer file)
    (call-with-output-file
      file
      (lambda (port)
	(put-string port buffer))
      '(truncate))))

(define apply-change
  (lambda (buffer change)
    (string-append
      buffer
      (change-value change))))

(define apply-changes
  (lambda (buffer changes)
    (fold-left
      apply-change
      buffer
      changes)))

(define edit
  (lambda (file)
    (set! *EXIT* #f)

    (save-file
      (call-with-values
	(lambda ()
	  (@loop-change (init-buffer file)))
	apply-changes)
      file)))

(define main
  (lambda (file)
    ; init editor
    (enable-raw-mode)
    (term-clear)
    (term-pin-cursor 1 1)

    (edit file)

    ; deinit editor
    (term-pin-cursor 1 1)
    (term-clear)
    (disable-raw-mode)
    ((lambda () (display #\newline)))))
