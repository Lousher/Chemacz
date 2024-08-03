(load-shared-object "raw-mode.so")

(define enable-raw-mode
  (foreign-procedure "enableRawMode" () void*))

(define disable-raw-mode
  (foreign-procedure "disableRawMode" () void*))

;---------------------------------
(define main
  (lambda (file)
    ; init editor
    (enable-raw-mode)
    (term-clear)
    (term-pin-cursor 1 1)
    (set! *BUFFER* #f)

    (edit file)

    ; deinit editor
    (set! *BUFFER* #f)
    (term-pin-cursor 1 1)
    (term-clear)
    (disable-raw-mode)
    ((lambda () (display #\newline)))))
