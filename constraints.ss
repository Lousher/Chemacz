(load "action.ss")

(define constrain
  (lambda (buf acts)
    (map
      (lambda (act)
	(constraints buf act))
      acts)))

; conditions
(define constraints
  (lambda (buf act)
    (cond
      [(move-down-at-last-line? buf act)
       (make-action "CONSTRAINT" #f #f)]
      [(move-right-at-last-char? buf act)
       (make-action "CONSTRAINT" #f #f)]
      [else act])))

(define move-down-at-last-line?
  (lambda (buf act)
    (and (ch-seq? (action-value act))
	 (let ([pos (action-position act)])
	   (and (= (+ (car pos) 1) (length buf))
		(string=? "DOWN" (ch-seq-type (action-value act))))))))

(define move-right-at-last-char?
  (lambda (buf act)
    (and (ch-seq? (action-value act))
	 (let* ([pos (action-position act)]
		[line (list-ref buf (car pos))])
	   (and (= (+ (cdr pos) 1) (rope-leaf-weight line))
		(string=? "RIGHT" (ch-seq-type (action-value act))))))))
