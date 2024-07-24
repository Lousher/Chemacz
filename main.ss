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

; for better manipulation buffer is like a list


(define init-buffer
  (lambda (file)
    (call-with-input-file
      file
      (lambda (port)
	(let ([content (get-string-all port)])
	  (if (eof-object? content)
	    '()
	    (reverse (string->list content))))))))

(define-record-type change
  (fields type value))

(define @get-change-from-user
  (lambda ()
    (let ([ch (read-char)])
      (case ch
	([#\x11]
	 (begin
	   (set! *EXIT* #t)
	   (make-change "EXIT" #f)))
	([#\delete] (make-change "DELETE" #f))
	(else
	  (make-change "ADD" ch))))))

(define file->string
  (lambda (file)
    (call-with-input-file
      file
      get-string-all)))

(define update-preview
  (lambda (preview change)
    (case (change-type change)
      (["ADD"] (display (change-value change)))
      (["DELETE"]
       (begin 
	 (term-cursor-left 1)
	 (term-delete-n-char-cursor-right 1))))))

; buffer now is list, and it should reverse?
; because in cons structure, car is new
(define @loop-change
  (lambda (buffer)
      (let loop
	([preview (display (list->string (reverse buffer)))]
	 [changes '()])
	(if *EXIT*
	  (values buffer changes)
	  (let ([change (@get-change-from-user)])
	    (loop (update-preview preview change)
		(append
		  (list change)
		  changes)))))))

; buffer now is list ch
(define save-file
  (lambda (buffer file)
    (call-with-output-file
      file
      (lambda (port)
	(put-string port (list->string (reverse buffer))))
      '(truncate))))

; buffer is list of ch
(define apply-change
  (lambda (change buffer)
    (case (change-type change) 
      	(["ADD"]
	 (append
	   (list (change-value change))
	   buffer))
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

    (edit file)

    ; deinit editor
    (term-pin-cursor 1 1)
    (term-clear)
    (disable-raw-mode)
    ((lambda () (display #\newline)))))
