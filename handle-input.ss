(load "char-sequence.ss")

(define-record-type action
  (fields type value))

(define @capture-ch-seq
  (lambda (ch seqs)
    (let loop ([chars (list ch)])
      (let-values
	([(status rest) (consume-ch-seq chars seqs)])
	(cond
	  ([ch-seq? status] status)
	  ([not status] chars)
	  (else (loop (append chars (list (read-char))))))))))

(define @capture-actions
  (lambda (buffer seqs)
    (let loop ([ch (read-char)])
      (if (char=? ch #\q)
	(list (make-action "EXIT" #f))
	(if (ch-seq-begin? ch seqs)
	  (let ([result (@capture-ch-seq ch seqs)])
	    (if (ch-seq? result)
	      (list (make-action "CH_SEQ" result))
	      (map
		(lambda (ch) (make-action "ADD" ch))
		result)))
	  (list (make-action "ADD" ch)))))))
