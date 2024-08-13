(load "string-extension.ss")

(define-record-type rope-leaf
  (fields
    (mutable weight)
    (mutable content))
  (protocol
    (lambda (new)
      (lambda (str)
	(let ([length (string-length str)])
	  (new length str))))))

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
