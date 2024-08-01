(load-shared-object "raw-mode.so")
(load "rope.ss")

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
(define *LINE_END* #\x5) ; C-E
(define *LINE_START* #\x1) ; C-A

(define line->rope-leaf
  (lambda (line)
    (make-rope-leaf line)))

(define lines->rope-leaves
  (lambda (lines)
    (if (null? lines)
      (list (make-rope-leaf ""))
      (map line->rope-leaf lines))))

(define rope-leaves->lines
  (lambda (leaves)
    (map rope-leaf-content leaves)))

(define file->lines
  (lambda (file)
    (call-with-input-file
      file
      (lambda (port)
	(let loop ([results '()]
		   [line (get-line port)])
	  (if (eof-object? line)
	    results
	    (loop
	      (append results (list line))
	      (get-line port))))))))

(define lines->string
  (lambda (lines)
    (fold-right
      (lambda (line result)
	(string-append
	   line (string #\newline) result))
      ""
      lines)))

; buffer is rope leaves, can be simple and small
(define init-buffer
  (lambda (file)
    (lines->rope-leaves
      (file->lines file))))

(define-record-type change
  (fields rope type value preview))

(define char-visible?
  (lambda (ch)
    (let ([code (char->integer ch)])
      (or (and (>= code #x20) (<= code #x7E))
	  (and (>= code #xA0) (<= code #xFFFD))))))

(define cur-rope
  (lambda (buffer)
    (let ([cur (term-get-cursor)])
      (if (> (car cur) (length buffer))
	(display "OUT")
	(cons
	  cur
	  (list-ref buffer (- (car cur) 1)))))))

(define make-control
  (lambda (ch buffer)
    (case ch
      ([#\x11]
       (begin
	 (set! *EXIT* #t)
	 (make-change #f "EXIT" #f (lambda (change) '()))))
      ([#\return]
       (make-change #f "RETURN" #f *preview-return))
      ([#\x5]
       (make-change
	 (cdr (cur-rope buffer))
	 "LINE_END"
	 #f
	 (lambda (change) (term-pin-cursor 1 (+ 1 (rope-leaf-weight (cdr (cur-rope buffer))))))))
      ([#\x1]
       (make-change
	 (cdr (cur-rope buffer))
	 "LINE_START"
	 #f
	 (lambda (change)
	   (term-pin-cursor 1 1))))
      ([#\delete]
       (make-change #f "DELETE" #f *preview-delete)))))

(define @get-change-from-user
  (lambda (buffer)
    (let ([ch (read-char)])
      (if (char-visible? ch)
	(make-change #f "ADD" ch *preview-add)
	(make-control ch buffer)
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
    (*display-lines (rope-leaves->lines buffer))
    (term-pin-cursor (car *CUR*) (cdr *CUR*))))

(define @loop-change
  (lambda (buffer)
    (let loop
      ([preview (*init-preview buffer)]
       [buffer-fin buffer])
      (if *EXIT*
	buffer-fin
	(let ([change (@get-change-from-user buffer)])
	  (loop ((change-preview change) change)
		(apply-change change buffer-fin)))))))

(define save-file
  (lambda (buffer file)
    (call-with-output-file
      file
      (lambda (port)
	(put-string port
		    (lines->string
		      (rope-leaves->lines buffer))))
      '(truncate))))

(define rope-weight-add
  (lambda (rope x)
    (let ([weight (rope-leaf-weight rope)])
      (rope-leaf-weight-set! rope (+ x weight)))))

(define delete-char 
  (lambda (cur-rope)
    (let ([cur (car cur-rope)]
	  [rope (cdr cur-rope)])
      (let ([length (rope-leaf-weight rope)]
	    [content (rope-leaf-content rope)])
	(rope-leaf-content-set!
	  rope
	  (string-truncate! content (- length 1))
	  )
	(rope-weight-add rope -1)))))

(define apply-change
  (lambda (change buffer)
    (case (change-type change) 
      (["ADD"]
       (let* ([rope-line (cdr (cur-rope buffer))]
	      [content (rope-leaf-content rope-line)])
	 (rope-leaf-content-set! rope-line
				 (string-append content (string (change-value change))))
	 (rope-weight-add rope-line 1)
	 buffer))
      (["DELETE"] 
       (begin
	 (delete-char (cur-rope buffer))
	 buffer))
      (["RETURN"]
       (let ([rows (length buffer)]
	     [cur (car (cur-rope buffer))])
	 (if (= (car cur) rows)
             (append buffer (list (make-rope-leaf "")))
	       buffer)))
      (["LINE_END"] buffer)
      (["EXIT"] buffer))))

(define edit
  (lambda (file)
    (set! *EXIT* #f)

    (save-file
      (@loop-change
	(init-buffer file))
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
