(load "string-extension.ss")
(define-record-type rope-leaf
  (fields
    (mutable weight)
    (mutable content))
  (protocol
    (lambda (new)
      (lambda (str)
	(let ([length (string-length str)])
	  (new length str)
	  )))))

(define-record-type rope-internal
  (fields
    (mutable weight)
    (mutable left)
    (mutable right)))

; ropes has following operations
; index, concatenate, split, insert, delete, substring, replace, length

(define display-rope
  (lambda (rope)
    (if (rope-leaf? rope)
      (display
	(rope-leaf-content rope))
      (begin
	(display-rope
	  (rope-internal-left rope))
	(display-rope
	  (rope-internal-right rope))))))

(define insert-rope-char!
  (lambda (rope cur ch)
    (let ([str (rope-leaf-content rope)]
	  [len (rope-leaf-weight rope)])
      (rope-leaf-content-set!
	rope (string-insert! str cur ch))
      (rope-leaf-weight-set! 
	rope (+ len 1)))))


(define delete-rope-char!
  (lambda (rope cur)
    (let ([str (rope-leaf-content rope)]
	  [len (rope-leaf-weight rope)])
      (rope-leaf-content-set! 
	rope (string-delete! str cur))
      (rope-leaf-weight-set!
	rope (- len 1)))))
