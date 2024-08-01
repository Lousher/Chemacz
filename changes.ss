(define-record-type change
  (fields rope type value preview))

(define make-control
  (lambda (ch buffer)
    (case ch
      ([#\x11]
       (begin
	 (set! *EXIT* #t)
	 (make-change #f "EXIT" #f (lambda (change) '()))))
      ([#\return]
       (make-change #f "RETURN" #f *preview-return))
      ([#\x5]
       (make-change
	 (cdr (cur-rope buffer))
	 "LINE_END"
	 #f
	 (lambda (change) (term-pin-cursor 1 (+ 1 (rope-leaf-weight (cdr (cur-rope buffer))))))))
      ([#\x1]
       (make-change
	 (cdr (cur-rope buffer))
	 "LINE_START"
	 #f
	 (lambda (change)
	   (term-pin-cursor 1 1))))
      ([#\delete]
       (make-change #f "DELETE" #f *preview-delete)))))


(define *preview-add
  (lambda (change)
    (display (change-value change))))

(define *preview-return
  (lambda (change)
    (begin
      (display #\return)
      (display #\newline))))

(define *preview-delete
  (lambda (change)
    (begin 
      (term-cursor-left 1)
      (term-delete-n-char-cursor-right 1))))

(define *display-lines
  (lambda (lines)
    (for-each
      display-line
      lines)))

(define *init-preview
  (lambda (buffer)
    (*display-lines (rope-leaves->lines buffer))
    (term-pin-cursor (car *CUR*) (cdr *CUR*))))
