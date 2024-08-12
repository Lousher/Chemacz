(load "action.ss")
(define constrain
  (lambda (buf acts)
    (map 
      (lambda (act)
	(no-right-in-line-end buf act))
      acts)))

; boundary for movement
(define no-right-in-line-end
  (lambda (buf act)
    (let ([pos (action-position act)])
      (let ([line (list-ref buf (car pos))])
	(if (and (= (rope-leaf-weight line)
	       (+ (cdr pos) 1))
	     (string=? "RIGHT" 
		       (ch-seq-type (action-value act))))
	  (make-action "CONSTRAINT" #f #f)
	  act)))))

