(load-shared-object "raw-mode.so")
(load "rope.ss")
(load "term-control.ss")
(load "changes.ss")

(define enable-raw-mode
  (foreign-procedure "enableRawMode" () void*))

(define disable-raw-mode
  (foreign-procedure "disableRawMode" () void*))

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
