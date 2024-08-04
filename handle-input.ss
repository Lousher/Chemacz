(load "escape-sequence.ss")

(define-record-type action
  (fields type value))

(define @capture-esc-seq
  (lambda (ch seqs)
    (let loop ([chars (list ch)])
      (let-values
	([(status rest) (consume-esc-seq chars seqs)])
	(cond
	  ([esc-seq? status] status)
	  ([not status] chars)
	  (else (loop (append chars (list (read-char))))))))))

(define @capture-actions
  (lambda (buffer seqs)
    (let loop ([ch (read-char)])
      (if (char=? ch #\q)
	(list (make-action "EXIT" #f))
	(if (esc-seq-begin? ch seqs)
	  (let ([result (@capture-esc-seq ch seqs)])
	    (if (esc-seq? result)
	      (list (make-action "ESC_SEQ" result))
	      (map
		(lambda (ch) (make-action "ADD" ch))
		result)))
	  (list (make-action "ADD" ch)))))))
