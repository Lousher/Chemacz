(load "string-extension.ss")
(load "rope.ss")

(define insert-buffer-char!
  (lambda (buf act)
    (let ([pos (action-position act)]
	  [val (action-value act)])
      (let ([rope (list-ref buf (car pos))])
	(insert-rope-char! rope (cdr pos) val)))))

(define delete-buffer-char!
  (lambda (buf act)
    (let ([pos (action-position act)])
      (let ([rope (list-ref buf (car pos))])
	(delete-rope-char! rope (cdr pos))))))

(define *refresh-delete-line
  (lambda (buf act)
    (let* ([pos (action-position act)]
	   [line (list-ref buf (car pos))])
      (term-pin-cursor (cons (car pos) 0))
      (display "\x1B;[2K")
      (display (rope-leaf-content line))
      (term-pin-cursor (action-position act))
      (term-cursor-left 1))))

(define *refresh-line
  (lambda (buffer action)
    (let* ([pos (action-position action)]
	   [line (list-ref buffer (car pos))])
      (term-pin-cursor (cons (car pos) 0))
      (display "\x1B;[2K")
      (display (rope-leaf-content line))
      (term-pin-cursor (action-position action))
      (term-cursor-right 1))))

(define apply-action
  (lambda (buffer action)
    (let ([type (action-type action)]
	  [value (action-value action)])
      (cond
	([string=? "NEWLINE" (action-type action)]
	 (begin 
	   (display "\x1B;E")))
	([string=? "INIT" (action-type action)]
	 (begin
	   (*display-buffer (action-value action))
	   (term-pin-cursor (action-position action))))
	([string=? "SAVE" (action-type action)]
	 (*SAVE* buffer))
	([string=? "DELETE" (action-type action)]
	 (begin
	   (delete-buffer-char! buffer action)
	   (*refresh-delete-line buffer action))
	 )
	([exit? action] (*EXIT*))
	([string=? "CONSTRAINT"
		   (action-type action)]
	 buffer)
	([ch-seq? value]
	 (begin
	   (term-display-seq 
	     (action-value action))))
	([add? action]
	 (begin
	   (insert-buffer-char! buffer action)
	   (*refresh-line buffer action)))))))

(define apply-actions
  (lambda (buffer actions)
    (fold-left
      apply-action
      buffer
      actions)))
