(load-shared-object "raw-mode.so")

(define enable-raw-mode
  (foreign-procedure "enableRawMode" () void*))

(define disable-raw-mode
  (foreign-procedure "disableRawMode" () void*))

(define (term-clear)
  (display "\x1B;[2J"))

(define (term-hide-cursor)
  (display "\x1B;[?25l"))

(define (term-show-cursor)
  (display "\x1B;[?25h"))

(define (term-pin-cursor x y)
  (for-each display (list #\esc #\[ x #\; y #\H)))

(define list-index
    (lambda (li i)
      (let loop ([rest li]
                 [index 0])
        (if (eqv? (car rest) i)
            index
            (loop (cdr rest) (+ index 1))))))

(define list-split
  (lambda (li i)
    (let ([index (list-index li i)])
      (cons (list-head li index)
	    (list-tail li (+ index 1))))))

(define (term-get-cursor)
  (call-with-port
    (current-input-port)
    (lambda (port)
      (display "\x1B;[6n")
      (let loop ([ch (read-char port)]
		 [ch-li '()])
	(if (char=? ch #\R)
	  (let ([li (list-split (list-tail ch-li 2) #\;)])
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
    (for-each display (list #\esc #\[ n #\P))))

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

(define line->rope-leaf
  (lambda (line)
    (make-rope-leaf line)))

(define lines->rope-leaves
  (lambda (lines)
    (map line->rope-leaf lines)))

; buffer is like a list
; buffer is lines with reverse order
(define file->lines
  (lambda (file)
    (call-with-input-file file
			  (lambda (port)
			    (let loop ([results '()]
				       [line (get-line port)])
			      (if (eof-object? line)
				results
				(loop
				  (cons line results)
				  (get-line port))))))))

(define lines->string
  (lambda (lines)
    (fold-right
      (lambda (line result)
	(string-append
	  result (string #\newline) line))
      ""
      lines)))

; buffer is rope structure, can be simple and small
(define init-buffer
  (lambda (file)
    (file->lines file)))

(define-record-type change
  (fields type value preview))

(define char-visible?
  (lambda (ch)
    (let ([code (char->integer ch)])
      (or (and (>= code #x20) (<= code #x7E))
	  (and (>= code #xA0) (<= code #xFFFD))))))

(define make-control
  (lambda (ch)
    (case ch
      ([#\x11]
       (begin
	 (set! *EXIT* #t)
	 (make-change "EXIT" #f (lambda (change) '()))))
      ([#\return]
       (make-change "RETURN" #f *preview-return))
      ([#\delete]
       (make-change "DELETE" #f *preview-delete)))))

(define @get-change-from-user
  (lambda ()
    (let ([ch (read-char)])
      (if (char-visible? ch)
	(make-change "ADD" ch *preview-add)
	(make-control ch)
	))))

(define file->string
  (lambda (file)
    (call-with-input-file
      file
      get-string-all)))

(define *preview-add
  (lambda (change)
    (display (change-value change))))

(define *preview-return
  (lambda (change)
    (begin
      (display #\return)
      (display #\newline))))

(define *preview-delete
  (lambda (change)
    (begin 
      (term-cursor-left 1)
      (term-delete-n-char-cursor-right 1))))

(define *display-lines
  (lambda (lines)
    (for-each
      display-line
      lines)))

(define *init-preview
  (lambda (buffer)
    (*display-lines buffer)
    (term-pin-cursor (car *CUR*) (cdr *CUR*))))

(define @loop-change
  (lambda (buffer)
    (let loop
      ([preview (*init-preview buffer)]
       [changes '()])
      (if *EXIT*
	(values buffer changes)
	(let ([change (@get-change-from-user)])
	  (loop ((change-preview change) change)
		(cons
		  change
		  changes)))))))

; buffer now is list lines
(define save-file
  (lambda (buffer file)
    (call-with-output-file
      file
      (lambda (port)
	(put-string port (lines->string buffer)))
      '(truncate))))

; buffer are lines like '("111" "222" "333") 
(define apply-change
  (lambda (change buffer)
    (case (change-type change) 
      (["ADD"]
       (if (null? buffer)
	 (cons (string (change-value change)) buffer)
	 (let ([line (car buffer)])
	   (set-car! buffer (string-append line (string (change-value change))))
	   buffer)))
      (["DELETE"] 
       (cdr buffer))
      (["RETURN"] buffer)
      (["EXIT"] buffer))))

(define apply-changes
  (lambda (buffer changes)
    (fold-right
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
    (set! *CUR* '(1 . 1))

    (edit file)

    ; deinit editor
    (term-pin-cursor 1 1)
    (term-clear)
    (disable-raw-mode)
    ((lambda () (display #\newline)))))
