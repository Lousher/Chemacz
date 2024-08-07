(define apply-action
  (lambda (buffer action)
    (let ([type (action-type action)]
	  [value (action-value action)])
      (cond
	([exit? action] (*EXIT*))
	([ch-seq? value]
	 buffer)
	([add? action]
	 buffer)))))

(define apply-actions
  (lambda (buffer actions)
    (fold-left
      apply-action
      buffer
      actions)))
