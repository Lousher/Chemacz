(load-shared-object "raw-mode.so")
(load "term-control.ss")
(load "port-extension.ss")
(load "rope.ss")
(load "input-to-action.ss")
(load "action-apply.ss")
(load "action-display.ss")

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

; *display must be after apply
(define edit
  (lambda (file)
    (call/cc 
      (lambda (exit)
	(set! *EXIT* exit)
	(set! *SAVE* (save-buf-to-file file))

	(let ([buffer (make-buffer file)])
	  (*display-buffer buffer)
	  (term-pin-cursor 0 0)
	  (let loop ([buf buffer])
	    (let ([actions (@capture-actions buf char-sequences)])
	      (loop
		(apply-actions buf actions)))))))))

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
    (term-pin-cursor 0 0)

    (edit file)

    ; deinit editor
    (term-pin-cursor 0 0)
    (term-clear)
    (disable-raw-mode)
    ))
