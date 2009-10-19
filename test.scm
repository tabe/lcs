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
(assert-lcs '(#\a #\d #\f #\h #\j)
            '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j)
            '(#\a #\0 #\1 #\d #\2 #\f #\3 #\h #\4 #\j))
(assert-lcs '(#\A #\( #\0 #\k #\[)
            '(#\A #\F #\W #\: #\{ #\` #\B #\v #\g #\0 #\] #\t #\( #\0 #\k #\> #\Y #\p #\- #\[)
            '(#\V #\4 #\6 #\e #\5 #\2 #\Z #\A #\N #\( #\T #\J #\o #\0 #\5 #\5 #\space #\k #\[ #\G))
(assert-lcs '(#\) #\s #\( #\= #\m)
            '(#\4 #\8 #\& #\space #\9 #\U #\z #\! #\2 #\E #\& #\M #\U #\. #\G #\^ #\O #\~ #\M #\X #\{ #\) #\^ #\s #\| #\! #\U #\j #\S #\O #\H #\L #\, #\^ #\C #\o #\O #\< #\( #\h #\| #\= #\m #\% #\; #\% #\! #\' #\K #\{)
            '(#\w #\t #\) #\s #\r #\( #\B #\2 #\g #\y #\$ #\c #\= #\F #\] #\s #\a #\x #\M #\m #\B #\n #\: #\i #\7 #\, #\R #\7 #\. #\e #\v #\V #\` #\: #\? #\P #\8 #\9 #\7 #\Z #\a #\b #\G #\3 #\8 #\p #\x #\y #\J #\r))
(assert-lcs '(#\s #\f #\~ #\H #\k #\D #\t #\w #\w #\} #\: #\M #\$ #\p #\_)
            '(#\W #\s #\9 #\a #\o #\= #\f #\U #\8 #\( #\" #\9 #\~ #\0 #\r #\b #\O #\B #\H #\k #\[ #\l #\D #\U #\_ #\_ #\p #\t #\w #\- #\Z #\w #\W #\R #\} #\; #\Y #\: #\G #\o #\j #\K #\h #\R #\y #\/ #\+ #\l #\6 #\k #\y #\7 #\t #\M #\m #\l #\0 #\K #\U #\v #\v #\r #\W #\u #\y #\J #\$ #\0 #\~ #\% #\d #\L #\? #\6 #\= #\> #\p #\< #\A #\: #\P #\i #\p #\X #\s #\_ #\_ #\7 #\- #\+ #\z #\t #\\ #\V #\Q #\e #\L #\M #\b #\,)
            '(#\_ #\Z #\, #\C #\3 #\< #\m #\7 #\t #\O #\{ #\B #\d #\J #\K #\S #\# #\_ #\2 #\z #\+ #\h #\\ #\z #\2 #\? #\s #\7 #\_ #\H #\# #\f #\G #\q #\b #\~ #\K #\\ #\d #\_ #\1 #\. #\H #\2 #\3 #\k #\U #\O #\@ #\C #\2 #\D #\q #\m #\S #\A #\e #\t #\i #\S #\w #\O #\y #\3 #\V #\W #\J #\T #\" #\d #\( #\e #\m #\n #\w #\P #\} #\E #\U #\M #\^ #\: #\c #\N #\M #\~ #\q #\q #\$ #\- #\; #\R #\p #\' #\2 #\@ #\_ #\] #\h #\`))
(assert-lcs '(#\* #\1 #\w #\& #\z #\= #\v #\f #\o #\2 #\b #\3 #\V #\: #\h #\i #\w #\1 #\, #\z #\u #\: #\z #\j #\C #\{ #\2 #\^ #\/ #\x)
            '(#\" #\t #\* #\m #\o #\1 #\K #\e #\g #\w #\7 #\* #\> #\< #\, #\t #\~ #\. #\p #\& #\! #\: #\i #\{ #\* #\) #\S #\1 #\z #\9 #\r #\Q #\? #\* #\0 #\a #\= #\Y #\v #\P #\` #\$ #\S #\\ #\Y #\" #\H #\; #\d #\4 #\2 #\4 #\` #\I #\f #\space #\b #\b #\o #\H #\' #\t #\P #\E #\2 #\y #\z #\S #\b #\C #\2 #\{ #\~ #\x #\7 #\X #\~ #\9 #\F #\= #\X #\^ #\3 #\6 #\k #\' #\C #\P #\~ #\: #\L #\Z #\A #\V #\# #\} #\K #\: #\L #\~ #\0 #\d #\h #\E #\y #\O #\1 #\h #\w #\i #\l #\O #\R #\A #\w #\1 #\$ #\| #\J #\K #\q #\y #\g #\, #\5 #\9 #\? #\z #\Z #\U #\8 #\: #\h #\P #\P #\y #\_ #\q #\( #\/ #\C #\i #\u #\: #\J #\s #\< #\X #\z #\r #\j #\f #\; #\! #\C #\f #\C #\+ #\i #\@ #\E #\H #\B #\N #\{ #\; #\@ #\_ #\| #\| #\D #\0 #\H #\7 #\n #\2 #\M #\^ #\% #\U #\k #\+ #\@ #\i #\C #\H #\/ #\/ #\5 #\? #\> #\+ #\C #\% #\P #\P #\+ #\x #\O #\()
            '(#\+ #\p #\# #\D #\2 #\7 #\U #\/ #\v #\j #\m #\S #\$ #\5 #\z #\O #\C #\z #\} #\@ #\u #\3 #\* #\1 #\/ #\- #\space #\s #\w #\2 #\] #\E #\space #\1 #\N #\5 #\& #\a #\m #\z #\^ #\= #\b #\z #\; #\7 #\S #\O #\G #\{ #\c #\v #\_ #\T #\D #\l #\f #\| #\5 #\! #\% #\z #\v #\o #\6 #\f #\[ #\y #\. #\j #\e #\W #\o #\8 #\6 #\2 #\8 #\) #\- #\c #\" #\N #\b #\> #\n #\m #\V #\R #\3 #\V #\: #\n #\h #\o #\5 #\q #\i #\Y #\V #\Z #\w #\} #\F #\/ #\` #\e #\m #\} #\1 #\6 #\- #\5 #\8 #\' #\I #\, #\A #\d #\z #\3 #\- #\3 #\; #\E #\u #\\ #\H #\O #\] #\m #\} #\4 #\: #\z #\1 #\) #\Y #\f #\. #\M #\( #\j #\M #\D #\Q #\4 #\] #\> #\, #\' #\l #\Z #\v #\| #\n #\* #\d #\| #\o #\L #\\ #\C #\R #\: #\{ #\2 #\a #\c #\< #\t #\^ #\$ #\J #\Q #\S #\= #\1 #\/ #\} #\B #\3 #\V #\m #\v #\8 #\' #\W #\' #\O #\0 #\E #\7 #\x #\r #\" #\D #\T #\f #\' #\f))

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
