;scheme test suite

;ok
(let ((TRUE (lambda (x) (lambda (y) x)))
      (FALSE (lambda (x) (lambda (y) y))))
  (let ((IF (lambda (p) (lambda (t) (lambda (e) (((p t) e)))))))
      (((IF FALSE) (lambda () 1)) (lambda () 2))))

;ok
(let* ((len-div-6 (lambda (len) (/ len 6)))
       (foo 
        (lambda (x) 
          (let ((y (len-div-6 x)))
            y))))
  (foo 12)) ;=> 2

;ok
(let* ((len-div-6 (lambda (len) (/ len 6)))
       (foo 
        (lambda (z) 
          (let ((x (len-div-6 z)))
            x))))
  (foo 12)) ;=> 2

;given that this works, it suggests the problem (after alpha renaming to x) is a lookup failure
;bad
(let* ((len-div-6 (lambda (len) (/ len 6)))
       (foo 
        (lambda (x) 
          (let ((x (len-div-6 x)))
            x))))
  (foo 12)) ;=> 2

;question:  is x in the environment; i.e. is this a lookup failure, or an environment making error?

;bad
(let ((len-div-6 (lambda (len) (/ len 6))))
  (let ((foo 
         (lambda (x) 
          (let ((x (len-div-6 x)))
           x))))
    (foo 12))) ;=> 2

;bad (stack overflow)
(let ((id (lambda (x) x)))
  (let ((foo (lambda (x) 
               (let ((x (id x)))
                 x))))
    (foo 12))) ;=> 12

;bad
(let* ((len-div-6 (lambda (len) (/ len 6)))
       (foo 
        (lambda (x) 
          (let* ((x (len-div-6 x)))
            x))))
  (foo 12)) ;=> 2

;ok
(let* ((len-div-6 (lambda (len) (/ len 6)))
       (foo 
        (lambda (x) 
		  ((lambda (x) (len-div-6 x)) 
		    x))))
  (foo 12)) ;=> 2

;ok (but fails in Dr. Racket)
(let* ((id (lambda (x) x))
       (foo 
	      (lambda (x) 
              ((lambda (x) 
                   (id x))
               x))))
  (foo 12)) ;=> 12

; this works
(let ((x 3))
	(let ((y 4))
		x))

((lambda (x)
	(let ((x 3))
		x) 2)

;this doesn't work:
(let* ((id (lambda (x) x))
       (foo 
        (lambda (x) 
          (let ((x (id x)))
            x))))
  (foo 12)) ;=> 12

;doesn't work (works in Dr. Racket)
(let* ((id (lambda (x) x))
        (foo 
            (lambda (x) 
               (let ((x (id x)))
                  x))))
  (foo 12)) ;=> 12

  ;doesn't work
  (define id (lambda (x) x))