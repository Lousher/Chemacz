(load-shared-object "raw-mode.so")
(load "term-control.ss")
(load "port-extension.ss")
(load "rope.ss")
(load "input-to-action.ss")
(load "action-apply.ss")
(load "constraints.ss")

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

(define *display-buffer
  (lambda (buf)
    (for-each
      term-display-line
      (map rope-leaf-content buf))))

(define buffer->string
  (lambda (buf)
    (fold-right
      (lambda (line res)
	(string-append line "\r\n" res))
      ""
      (map rope-leaf-content buf))))

(define save-buf-to-file
  (lambda (file)
    (lambda (buf)
      (call-with-output-file
	file
	(lambda (port)
	  (put-string
	    port
	    (buffer->string buf)))
	'(truncate)))))

(define *init-display
  (lambda (buf)
    (make-action "INIT" buf '(0 . 0))))

(define edit
  (lambda (file)
    (call/cc 
      (lambda (exit)
	(set! *EXIT* exit)
	(set! *SAVE* (save-buf-to-file file))
	(let ([buffer (make-buffer file)])
	  (let loop ([acts (list (*init-display buffer))])
	    (apply-actions buffer acts)
	    (loop
	      (constrain
		buffer
		(@capture-actions char-sequences)))))))))

(define char-sequences
  (call-with-input-file
    "char-sequences.txt"
    (lambda (port)
      (map make-ch-seq 
	   (get-lines port)))))

(define main
  (lambda (file)
    ; init editor
    (enable-raw-mode)
    (term-clear)
    (term-pin-cursor '(0 . 0))

    (edit file)

    ; deinit editor
    (term-pin-cursor '(0 . 0))
    (term-clear)
    (disable-raw-mode)))
