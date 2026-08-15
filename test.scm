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

(import
 (scheme base) (scheme write) (scheme read) (srfi 41) (srfi 64) (srfi 1) (scheme inexact) (scheme file) (scheme load)
 (chariot curves) (chariot fool) (chariot read) (chariot config) (judgement) (chariot render) (chariot codec))


(define (check-stream=list s l tester)
 (cond
  [(stream-null? s) (test-eq l '())]
  [(null? l) (test-assert (stream-null? s))]
  [else (tester (stream-car s) (car l)) (check-stream=list (stream-cdr s) (cdr l) tester)]))

#;
(test-group "bezier"
 (parameterize [[sample-rate 44100]]
  (check-stream=list (bezier 44100 '(0 . 0) '(0.2 . -0.3) '(0.3 . 0.4) '(0.7 . 0.1) '(0.6 . 0.7) '(1 . 1)) (read (open-input-file "curve1")) (lambda (a b) (test-approximate b a 1e-9)))
  (check-stream=list (bezier 44100 '(0 . 1) '(0.5 . 0.2) '(0.2 . 0.3) '(0.7 . 0.1) '(0.8 . -1) '(1 . 0)) (read (open-input-file "curve2")) (lambda (a b) (test-approximate b a 1e-9)))
  (test-error "Bad curve" (bezier 3 '(0 . 1) '(0.6 . 0.2) '(0.6 . 0.4) '(0.8 . 0.3) '(0.1 . -0.8) '(1 . 0)))))

(test-group "fool"
 (let [[scales '((#\p (octave-align . 3) (octave-rate . (+ 1 1)) ("C-" . 1) ("G-" . 3/2) ("F!" . (expt 2 5/12))))]]
  (test-eqv (notevector->freq #("G-" 4 #\p) scales) 396)))
;#;
(test-group "curve reader"
 (parameterize [[sample-rate 44100]]
  (let* [[head1 (read (open-input-file "example.head"))]
         [head2 (read (open-input-file "all.head"))]
         [s-notes (read (open-input-file "example.notes"))]
         [head (get-head head1 head2)]
         [notes (get-notes s-notes head)]]
   (define (curve-checker a b)
    (if (procedure? a)
     (test-eq 'noflag-record!!! b)
     (test-approximate b a 1e-9)))
   (check-stream=list (get-curve 'freq notes head)
    (read (open-input-file "freq.curve"))
    curve-checker)
   (check-stream=list (get-curve #\v notes head)
    (read (open-input-file "v.curve"))
    curve-checker))))

(test-group "Engine interface"
 (let* ([head1 (read (open-input-file "example.head"))]
        [head2 (read (open-input-file "all.head"))]
        [head (get-head head1 head2)]
        [notes (get-notes (read (open-input-file "example.notes")) head)]
        [s (render-channel head notes)]
        [lst (stream->list s)]
        [expected (list (list (cons 'wish1 "Hail 2 U!"))
                        (cons 'name 'wish1)
                        (cons 'flags '())
                        (cons 'sr (sample-rate)))])
  (test-equal lst expected)))

(test-group "merge-channels"
  (let* ([s1 (constant-line 3 1)]
         [s2 (constant-line 3 2)]
         [res (merge-channels (list s1 s2) (list 1/4 1/2))])
    ;; expect 1*1/4 + 2*1/2 = 1.25 for each frame
    (check-stream=list res (list 1.25 1.25 1.25) (lambda (a b) (test-approximate b a 1e-9)))
    ;; error when vols sum to more than 1
    (test-error "Sum of vols more than 1" (merge-channels (list s1 s2) (list 1 1)))))

(test-group "codec"
  ;; Use parameterize with the config parameters instead of positional args
  (parameterize ([signed #t] [byte-depth 1] [big-endian #t])
    ;; 1-byte depth: choose rationals that scale to exact integers so explode is deterministic
    ;; 1/127 -> scaled = 1 -> remainder 1
    (let* ([bv1 (codec (list (/ 1 127)))]
           [lst1 (map (lambda (i) (bytevector-u8-ref bv1 i)) (iota (bytevector-length bv1)))])
      (test-equal lst1 (list 1)))

    ;; full-scale 1 -> scaled = 127 -> remainder 7
    (let* ([bv2 (codec (list 1))]
           [lst2 (map (lambda (i) (bytevector-u8-ref bv2 i)) (iota (bytevector-length bv2)))])
      (test-equal lst2 (list 7)))

    ;; negative full-scale -1 should be accepted (range now -1..1) and produce a deterministic byte
    (let* ([bv_neg (codec (list -1))]
           [lst_neg (map (lambda (i) (bytevector-u8-ref bv_neg i)) (iota (bytevector-length bv_neg)))])
      (test-equal lst_neg (list 1)))

    ;; two samples at 1-byte depth: 1 maps to 7, 0 maps to 0
    (let* ([bv3 (codec (list 1 0))]
           [lst3 (map (lambda (i) (bytevector-u8-ref bv3 i)) (iota (bytevector-length bv3)))])
      (test-equal lst3 (list 7 0))))

  (parameterize ([signed #t] [byte-depth 2] [big-endian #t])
    ;; 2-byte depth: 1/32767 -> scaled = 1 -> exploded -> (0 1) for big-endian
    (let* ([bv4 (codec (list (/ 1 32767)))]
           [lst4 (map (lambda (i) (bytevector-u8-ref bv4 i)) (iota (bytevector-length bv4)))])
      (test-equal lst4 (list 0 1))))

  (parameterize ([signed #t] [byte-depth 1] [big-endian #t])
    ;; multiple fractional samples producing exact scaled integers
    (let* ([bv5 (codec (list (/ 64 127) (/ 63 127)))]
           [lst5 (map (lambda (i) (bytevector-u8-ref bv5 i)) (iota (bytevector-length bv5)))])
      (test-equal lst5 (list 0 7))))

  ;; out-of-range (too large and too negative) should raise errors
  (test-error "Data out of range (large)" (parameterize ([signed #t] [byte-depth 1] [big-endian #t]) (codec (list 2))))
  (test-error "Data out of range (too negative)" (parameterize ([signed #t] [byte-depth 1] [big-endian #t]) (codec (list -2)))))

