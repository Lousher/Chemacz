(load "port-extension.ss")
(load "string-extension.ss")

; like UP esc [ A
(define-record-type ch-seq
  (fields chars type)
  (protocol
    (lambda (new)
      (lambda (line)
	(let ([items (string-split line #\space)])
	  (new 
	    (map string->char (cdr items))
	    (car items)))))))


(define list-nth-eq?
  (lambda (list index item)
    (eq? item (list-ref list index))))

(define ch-seq-nth-eq?
  (lambda (seq index ch)
    (let ([chars (ch-seq-chars seq)])
      (if (< index (length chars))
	(list-nth-eq? chars index ch)
	#t))))

(define filter-ch-seqs-nth-eq
  (lambda (seqs index ch)
    (filter
      (lambda (seq)
	(ch-seq-nth-eq? seq index ch))
      seqs)))

(define int-range
  (lambda (from to)
    (if (= from to)
      (list to)
      (cons from (int-range (+ from 1) to)))))

(define filter-ch-seqs-chars-eq
  (lambda (seqs chars)
    (fold-left
      filter-ch-seqs-nth-eq
      seqs
      (int-range 0 (- (length chars) 1))
      chars)))

(define only?
  (lambda (li)
    (= 1 (length li))))

(define consume-ch-seq
  (lambda (chars seqs)
    (let ([alters (filter-ch-seqs-chars-eq seqs chars)])
      (cond
	[(only? alters)
	 (values
	   (car alters)
	   (list-tail
	     chars
	     (length (ch-seq-chars (car alters)))))]
	[(null? alters) (values #f chars)]
	[else (values alters chars)]))))

(define list-last
  (lambda (li)
    (let ([len (length li)])
      (list-ref li (- len 1)))))

(define ch-seq-begin?
  (lambda (ch seqs)
    (let ([begin-chars (map car (map ch-seq-chars seqs))])
      (memq ch begin-chars))))
