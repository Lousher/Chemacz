(define list-split
    (lambda (li ch)
      (let loop ([li1 '()]
                 [li2 li])
        (if (char=? ch (car li2))
            (cons li1 (cdr li2))
            (loop
              (append li1 (list (car li2)))
              (cdr li2))))))

(define list-insert
    (lambda (li n item)
      (let ([head (list-head li n)]
            [tail (list-tail li n)])
        (append head (list item) tail))))
