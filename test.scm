#| test.scm -- this file is used in the development of CHARIOT: Curves, Hackability And Restriction-less Instrument Oriented Tracker
 | Copyright (C) 2026 wqy24
 | This file is under the same license as CHARIOT
 |
 | CHARIOT is free software: you can redistribute it and/or modify
 | it under the terms of the GNU General Public License as published by
 | the Free Software Foundation, either version 3 of the License, or
 | (at your option) any later version.
 |
 | CHARIOT is distributed in the hope that it will be useful,
 | but WITHOUT ANY WARRANTY; without even the implied warranty of
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 | GNU General Public License for more details.
 |
 | You should have received a copy of the GNU General Public License
 | along with CHARIOT. If not, see <https://www.gnu.org/licenses/>.
 |#

(import (scheme base) (scheme write) (scheme read) (srfi 41) (srfi 64) (scheme inexact) (scheme file) (chariot curves) (chariot fool) (chariot read) (chariot config) (judgement) (chariot render))


(define (check-stream=list s l tester)
 (cond
  [(stream-null? s) (test-eq l '())]
  [(null? l) (test-assert (stream-null? s))]
  [else (tester (stream-car s) (car l)) (check-stream=list (stream-cdr s) (cdr l) tester)]))

(test-group "bezier"
 (check-stream=list (bezier 44100 '(0 . 0) '(0.2 . -0.3) '(0.3 . 0.4) '(0.7 . 0.1) '(0.6 . 0.7) '(1 . 1)) (read (open-input-file "curve1")) (lambda (a b) (test-approximate b a 1e-9)))
 (check-stream=list (bezier 44100 '(0 . 1) '(0.5 . 0.2) '(0.2 . 0.3) '(0.7 . 0.1) '(0.8 . -1) '(1 . 0)) (read (open-input-file "curve2")) (lambda (a b) (test-approximate b a 1e-9)))
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
   (if (procedure? a)
    (test-eq 'noflag-record!!! b)
    (test-approximate b a 1e-9)))
  (check-stream=list (get-curve 'freq notes head)
   (read (open-input-file "freq.curve"))
   curve-checker)
  (check-stream=list (get-curve #\v notes head)
   (read (open-input-file "v.curve"))
   curve-checker)))

(test-group "Engine interface"
 (let* ([head1 (open-input-file "example.head")]
        [head2 (open-input-file "all.head")]
        [head (get-head head1 head2)]
        [notes (get-notes (open-input-file "example.notes") head)]
        [s (render-channel head notes)]
        [lst (stream->list s)]
        [expected (list (list (cons 'wish1 "Hail 2 U!"))
                        (cons 'name 'wish1)
                        (cons 'flags '())
                        (cons 'sr SAMPLE-RATE))])
  (test-equal lst expected)))
