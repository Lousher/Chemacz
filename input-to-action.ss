(load "char-sequence.ss")
(load "term-control.ss")

(define-record-type action
  (fields type value position))

(define @capture-ch-seq
  (lambda (ch seqs)
    (let loop ([chars (list ch)])
      (let-values
	([(status rest) (consume-ch-seq chars seqs)])
	(cond
	  ([ch-seq? status] status)
	  ([not status] chars)
	  (else (loop (append chars (list (read-char))))))))))

(define list-wrap
  (lambda (obj)
    (if (list? obj)
      obj
      (list obj))))

(define @capture-actions
  (lambda (seqs)
    (let ([ch (read-char)])
      (cond
	[(char=? ch #\q)
	 (list (make-action "EXIT" #f #f))]
	[(char=? ch #\x13)
	 (list (make-action "SAVE" #f #f))]
	[(ch-seq-begin? ch seqs)
	 (let ([result (@capture-ch-seq ch seqs)])
	   (if (ch-seq? result)
	     (list (make-action "CH_SEQ" result #f))
	     (map
	       (lambda (ch) (make-action "ADD" ch (term-get-cursor)))
	       result)))]
	[else (list (make-action "ADD" ch (term-get-cursor)))]))))
