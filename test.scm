#!r6rs

(import (rnrs) (lcs) (xunit))

(define-syntax assert-ses
  (syntax-rules ()
    ((_ expected-n expected-s a b opt ...)
     (call-with-values
         (lambda () (ses a b opt ...))
       (lambda (n s)
         (assert-= expected-n n)
         (assert-equal? expected-s s))))))

(define-syntax assert-lcs
  (syntax-rules ()
    ((_ expected a b opt ...)
     (assert-equal? expected (lcs a b opt ...)))))

(define-syntax assert-lcs-with-positions
  (syntax-rules ()
    ((_ expected a b opt ...)
     (assert-equal? expected (lcs-with-positions a b opt ...)))))

(define-syntax assert-lcs-edit-list
  (syntax-rules ()
    ((_ expected a b opt ...)
     (assert-equal? expected (lcs-edit-list a b opt ...)))))

(assert-ses 0 '() '() '())
(assert-ses 1 '(-1) '(a) '())
(assert-ses 1 '(1) '() '(a))
(assert-ses 0 '(0) '(a) '(a))
(assert-ses 7 '(1 0 0 1 1 -1 -1 -1 0 1) '(a b d e 2 f) '(0 a b 1 c f g))

(assert-lcs '(a b) '(a b c) '(a b a))
(assert-lcs '(a b a d) '(a b a c d) '(a e b a d))
(assert-lcs '(#\a #\b) '(#\a #\b #\c) '(#\A #\B #\A) char-ci=?)

(assert-lcs-with-positions '(1 ((a 0 0))) '(a) '(a))
(assert-lcs-with-positions '(2 ((a 1 2) (b 2 3))) '(x a b y) '(p q a b))
(assert-lcs-with-positions '(2 ((a 1 1) (b 2 3))) '(x a b y) '(p a q b))
(assert-lcs-with-positions '(0 ()) '(x y) '(p q))

(assert-lcs-edit-list
 '(((- 0 "A"))
   ((+ 2 "D"))
   ((+ 4 "F") (- 4 "H"))
   ((+ 6 "K"))
   ((+ 9 "R") (+ 10 "S") (+ 11 "T") (- 8 "N") (- 9 "P"))
   )
 '("A" "B" "C" "E" "H" "J" "L" "M" "N" "P")
 '("B" "C" "D" "E" "F" "J" "K" "L" "M" "R" "S" "T")
 )

(report)
