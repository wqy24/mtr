(import (scheme base) (scheme write) (scheme read) (srfi 41) (srfi 64) (scheme inexact) (chariot write-sample) (scheme file) (chariot curves) (chariot fool) (chariot read))

(define (force-list l)
 (if (stream-pair? l) (cons (stream-car l) (force-list (stream-cdr l))) '()))

(write-samples! '(1 222 3 4444) (open-binary-output-file "x.raw"))

(test-group "bezier"
 (for-each (lambda (a b) (test-approximate b a 1e-9)) (force-list (bezier 44100 '(0 . 0) '(0.2 . -0.3) '(0.3 . 0.4) '(0.7 . 0.1) '(0.6 . 0.7) '(1 . 1))) (read (open-input-file "curve1")))
 (for-each (lambda (a b) (test-approximate b a 1e-9)) (force-list (bezier 44100 '(0 . 1) '(0.5 . 0.2) '(0.2 . 0.3) '(0.7 . 0.1) '(0.8 . -1) '(1 . 0))) (read (open-input-file "curve2")))
 (test-error "Bad curve" (bezier 3 '(0 . 1) '(0.6 . 0.2) '(0.6 . 0.4) '(0.8 . 0.3) '(0.1 . -0.8) '(1 . 0))))

(test-group "fool"
 (let [[scales '((#\p (octave-align . 3) (octave-rate . (+ 1 1)) ("C-" . 1) ("G-" . 3/2) ("F!" . (expt 2 5/12))))]]
  (test-eqv (notevector->freq #("G-" 4 #\p) scales) 396)))

(test-group "curve reader"
 (let* [[head1 (open-input-file "example.head")]
        [head2 (open-input-file "all.head")]
        [p-notes (open-input-file "example.notes")]
        [head (get-head head1 head2)]
        [notes (get-notes p-notes head)]]
  (define (curve-checker a b)
   (if (noflag? a)
    (test-eq 'noflag-record!!! b)
    (test-approximate b a 1e-9)))
  (for-each curve-checker
   (force-list (get-curve 'freq notes head))
   (read (open-input-file "freq.curve")))
  (for-each curve-checker
   (force-list (get-curve #\v notes head))
   (read (open-input-file "v.curve")))))
