(load-shared-object "raw-mode.so")
(load "term-control.ss")
(load "port-extension.ss")
(load "rope.ss")
(load "handle-input.ss")

(define enable-raw-mode
  (foreign-procedure "enableRawMode" () void*))

(define disable-raw-mode
  (foreign-procedure "disableRawMode" () void*))

;---------------------------------
(define make-buffer
  (lambda (file)
    (call-with-input-file
      file
      (lambda (port)
	(map make-rope-leaf
	     (get-lines port))))))

(define exit?
  (lambda (action)
    (string=? "EXIT" (action-type action))))

(define add?
  (lambda (action)
    (string=? "ADD" (action-type action))))

; using call/cc to break, more beautiful
(define apply-action
  (lambda (buffer action)
    (let ([type (action-type action)]
	  [value (action-value action)])
      (cond
	([exit? action] (*EXIT*))
	([esc-seq? value]
	 (term-exec-esc-seq value))
	([add? action]
	 (term-display (action-value action)))))))

(define apply-actions
  (lambda (buffer actions)
    (fold-left
      apply-action
      buffer
      actions)))

; esc-seqs is in escape-sequences.ss
(define edit
  (lambda (file)
    (call/cc 
      (lambda (exit)
	(set! *EXIT* exit)
	(let loop ([buffer (make-buffer file)])
	  (let ([actions (@capture-actions buffer esc-seqs)])
	    (loop (apply-actions buffer actions))))))))

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
    ((lambda () (display "DONE")))))
