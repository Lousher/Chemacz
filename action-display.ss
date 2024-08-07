(define *display-action
  (lambda (buffer action)
    (let ([type (action-type action)]
	  [value (action-value action)])
      (cond
	([exit? action] '())
	([ch-seq? value]
	 (term-display-seq value))
	([add? action]
	 (term-display
	   (action-value action)))))))     

(define *display-actions
  (lambda (buf actions)
    (for-each
      (lambda (act)
	(*display-action buf act))
      actions)))
