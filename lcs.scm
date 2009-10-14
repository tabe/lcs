;;
;;   Copyright (c) 2009 Takeshi Abe. All rights reserved.
;;
;;   Redistribution and use in source and binary forms, with or without
;;   modification, are permitted provided that the following conditions
;;   are met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;
;;    3. Neither the name of the authors nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;
;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(library (lcs)
  (export ses
          lcs-fold
          lcs
          lcs-with-positions
          lcs-edit-list)
  (import (rnrs))

  ;; cf. S. Wu, U. Manber, G. Myers, and W. Miller, "An O(NP) Sequence Comparison Algorithm" (1989)
  (define (ses a b . opt-eq)
    (let* ((eq-proc (if (null? opt-eq) equal? (car opt-eq)))
           (m (length a))
           (n (length b))
           (u (if (<= m n) 1 -1))
           (M (min m n))
           (N (max m n))
           (A (list->vector (cons #f (if (<= m n) a b))))
           (B (list->vector (cons #f (if (<= m n) b a))))
           (M+1 (+ M 1))
           (D (- N M))
           (fp (make-vector (+ M N 3) -1))
           (path (make-vector (+ M N 3) '())))

      (letrec-syntax ((fp-ref (syntax-rules () ((_ i) (vector-ref fp (+ i M+1)))))
                      (fp-set! (syntax-rules () ((_ i v) (vector-set! fp (+ i M+1) v))))
                      (path-ref (syntax-rules () ((_ i) (vector-ref path (+ i M+1)))))
                      (path-set! (syntax-rules () ((_ i v) (vector-set! path (+ i M+1) v))))
                      (path-push! (syntax-rules () ((_ i v) (path-set! i (cons v (path-ref i)))))))

        (define (snake! k)
          (let* ((y1 (+ (fp-ref (- k 1)) 1))
                 (y2 (fp-ref (+ k 1)))
                 (y (max y1 y2)))
            (cond ((> y1 y2)
                   (path-set! k (map values (path-ref (- k 1))))
                   (path-push! k u))
                  (else
                   (path-set! k (map values (path-ref (+ k 1))))
                   (path-push! k (- u))))
            (let loop ((x (- y k))
                       (y y))
              (cond ((and (< x M)
                          (< y N)
                          (eq-proc (vector-ref A (+ x 1))
                                   (vector-ref B (+ y 1))))
                     (path-push! k 0)
                     (loop (+ x 1) (+ y 1)))
                    (else (fp-set! k y))))))

        (let p-loop ((p 0))
          (do ((k (- p) (+ k 1)))
              ((= k D))
            (snake! k))
          (do ((k (+ D p) (- k 1)))
              ((= k D))
            (snake! k))
          (snake! D)
          (if (= (fp-ref D) N)
              (values (+ D (* 2 p)) (cdr (reverse (path-ref D))))
              (p-loop (+ p 1)))))))

  (define (lcs-fold a-proc b-proc both-proc seed a b . opt-eq)
    (call-with-values
        (lambda () (apply ses a b opt-eq))
      (lambda (_ s)
        (let loop ((a a)
                   (i 0)
                   (b b)
                   (j 0)
                   (s s)
                   (seed seed))
          (if (null? s)
              seed
              (case (car s)
                ((0)
                 (loop (cdr a)
                       (+ i 1)
                       (cdr b)
                       (+ j 1)
                       (cdr s)
                       (both-proc (car a) seed)))
                ((1)
                 (loop a
                       i
                       (cdr b)
                       (+ j 1)
                       (cdr s)
                       (b-proc (car b) seed)))
                (else
                 (loop (cdr a)
                       (+ i 1)
                       b
                       j
                       (cdr s)
                       (a-proc (car a) seed)))))))))

  (define (lcs a b . opt-eq)
    (let ((pass (lambda (x seed) seed)))
      (reverse (apply lcs-fold pass pass cons '() a b opt-eq))))

  (define (lcs-with-positions a b . opt-eq)
    (call-with-values
        (lambda () (apply ses a b opt-eq))
      (lambda (_ s)
        (let loop ((a a)
                   (i 0)
                   (b b)
                   (j 0)
                   (s s)
                   (n 0)
                   (r '()))
          (if (null? s)
              (list n (reverse r))
              (case (car s)
                ((0)
                 (loop (cdr a)
                       (+ i 1)
                       (cdr b)
                       (+ j 1)
                       (cdr s)
                       (+ n 1)
                       (cons (list (car a) i j) r)))
                ((1)
                 (loop a
                       i
                       (cdr b)
                       (+ j 1)
                       (cdr s)
                       n
                       r))
                (else
                 (loop (cdr a)
                       (+ i 1)
                       b
                       j
                       (cdr s)
                       n
                       r))))))))

  (define (lcs-edit-list a b . opt-eq)
    (call-with-values
        (lambda () (apply ses a b opt-eq))
      (lambda (_ s)
        (let loop ((a a)
                   (i 0)
                   (b b)
                   (j 0)
                   (s s)
                   (hunk '())
                   (hunks '()))
          (if (null? s)
              (reverse
               (if (null? hunk) hunks (cons (reverse hunk) hunks)))
              (case (car s)
                ((0)
                 (loop (cdr a)
                       (+ i 1)
                       (cdr b)
                       (+ j 1)
                       (cdr s)
                       '()
                       (if (null? hunk) hunks (cons (reverse hunk) hunks))))
                ((1)
                 (loop a
                       i
                       (cdr b)
                       (+ j 1)
                       (cdr s)
                       `((+ ,j ,(car b)) ,@hunk)
                       hunks))
                (else
                 (loop (cdr a)
                       (+ i 1)
                       b
                       j
                       (cdr s)
                       `((- ,i ,(car a)) ,@hunk)
                       hunks))))))))

)
