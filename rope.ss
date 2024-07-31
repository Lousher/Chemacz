; rope leaf size will be a total line

(define-record-type rope-leaf
  (fields
    (mutable weight)
    (mutable content))
  (protocol
    (lambda (new)
      (lambda (str)
	(let ([length (string-length str)])
	  (new length (string->immutable-string str))
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
