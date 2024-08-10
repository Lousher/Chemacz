(define string-insert!
  (lambda (str pos ch)
    (let ([len (string-length str)])
      (if (eq? pos len)
	(string-append str (string ch))
	(let ([str1 (substring str 0 pos)]
	      [str2 (substring str pos len)])
	  (string-append str1 (string ch) str2)
	  )))))

(define insert-rope-char!
  (lambda (rope cur ch)
    (let ([str (rope-leaf-content rope)]
	  [len (rope-leaf-weight rope)])
      (rope-leaf-content-set!
	rope (string-insert! str cur ch))
      (rope-leaf-weight-set! 
	rope (+ len 1))
      )))

(define insert-buffer-char!
  (lambda (buf act)
    (let ([pos (action-position act)]
	  [val (action-value act)])
      (let ([rope (list-ref buf (car pos))])
	(insert-rope-char! rope (cdr pos) val)
	))))

(define apply-action
  (lambda (buffer action)
    (let ([type (action-type action)]
	  [value (action-value action)])
      (cond
	([string=? "SAVE" (action-type action)]
	 (*SAVE* buffer))
	([exit? action] (*EXIT*))
	([ch-seq? value]
	 buffer)
	([add? action]
	 (begin 
	   (insert-buffer-char! buffer action)
	   buffer))))))

  (define apply-actions
    (lambda (buffer actions)
      (fold-left
	apply-action
	buffer
	actions)))
