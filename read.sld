(define-library (chariot read)
 (import (scheme base) (scheme read) (wqy24 assert) (wqy24 debug) (chariot config) (chariot fool) (chariot curves) (srfi 41) (srfi 132) (srfi 1) (srfi 133))
 (export get-curve noflag? get-head get-notes)
 (begin
  (define-record-type curve-record-record
   (curve-record processor v1 c1 v2 c2 r)
   curve-record?
   [processor processor]
   [v1 value1]
   [c1 curve1]
   [v2 value2]
   [c2 curve2]
   [r repeats])

  (define (get-curve-record desc proc)
   (let*-values [[[v1 c1 v2 c2 r] (apply values desc)]
                 [[front end] (if (> v1 v2) (values 1 0) (values 0 1))]]
    (curve-record
     proc
     v1
     `((0 . ,front) ,@c1 (1 . ,end))
     v2
     `((0 . ,end) ,@c2 (1 . ,front))
     r)))

  (define (get-head port1 port2)
   (append (read port1) (read port2)))

  (define (get-ticks port)
   (let [[sexp (read port)]]
    (if (eof-object? sexp)
     stream-null
     (stream-cons sexp (get-ticks port)))))

  (define (get-notes port head)
   (define frames/tick (truncate (* SAMPLE-RATE (cdr (assq 'tempo head)))))
   (define ticks (get-ticks port))
   (let next-tick [[cticks ticks]]
    (if (stream-null? cticks)
     stream-null
     (let [[curr-tick (stream-car cticks)]]
      (let next-frame [[frames 0] [notes (list-sort (lambda (a b) (<= (car a) (car b))) curr-tick)]]
       (if (>= frames frames/tick)
        (next-tick (stream-cdr cticks))
        (let [[result (if (or (not (pair? notes)) (< frames (* SAMPLE-RATE (caar notes)))) #f (car notes))]]
         (stream-cons result (next-frame (+ frames 1) (if result (cdr notes) notes))))))))))

  (define (curve-length name notes) ; in frames
   (assert notes stream-car "Notes should starts with a note start")
   (let loop [[len 1] [nnotes (stream-cdr notes)]]
    (cond
     [(stream-null? nnotes) len]
     [(not (stream-car nnotes)) (loop (+ len 1) (stream-cdr nnotes))]
     [(and (eq? name 'freq) (cadr (stream-car nnotes))) len]
     [(assv name (cddr (stream-car nnotes))) len]
     [else (loop (+ len 1) (stream-cdr nnotes))])))

  (define (expt-proc higher lower v)
   (* lower (expt (/ higher lower) v)))

  (define (linear-proc higher lower v)
   (+ lower (* (- higher lower) v)))

  (define (points->curve pts len proc higher lower)
   (assert pts (lambda (p) (<= (length pts) 6)) "Too many control points")
   (let loop [[apps (- 6 (length pts))] [p pts]]
    (if (zero? apps)
     (stream-map (lambda (v) (proc higher lower v))
      (apply bezier len p))
     (loop (- apps 1) (cons* (car p) (cadr p) (cdr p))))))

  (define (curve-record->curve record len)
   (define lens
    (let loop [[c-lens '()] [rlen len] [c-rep (- (repeats record) 1)]]
     (if (zero? c-rep)
      (cons rlen c-lens)
      (let [[clen (truncate (/ len (repeats record)))]]
       (loop (cons clen c-lens) (- rlen clen) (- c-rep 1))))))
   (let loop [[lens lens]
              [curver curve1]
              [c-curve #f]]
    (cond
     [(and (null? lens) (not c-curve)) stream-null]
     [(not c-curve)
      (let*-values [[[v1 v2]
                     (values (value1 record) (value2 record))]
                    [[higher lower]
                     (if (> v1 v2)
                      (values v1 v2)
                      (values v2 v1))]]
       (loop
        (cdr lens)
        curver
        (points->curve (curver record) (car lens) (processor record) higher lower)))]
     [(stream-null? c-curve)
      (loop
       lens
       (if (eq? curver curve1)
        curve2 curve1)
       #f)]
     [else
      (stream-cons (stream-car c-curve) (loop lens curver (stream-cdr c-curve)))])))

  (define (curve desc proc len)
   (curve-record->curve (get-curve-record desc proc) len))

  (define-record-type noflag-record
   (noflag)
   noflag?)

  (define (get-curve name notes head)
   (let loop [[c-notes notes] [c-curve stream-null]]
    (cond
     [(stream-null? c-notes) stream-null]
     [(stream-pair? c-curve) (stream-cons (stream-car c-curve) (loop (stream-cdr c-notes) (stream-cdr c-curve)))]
     [(not (stream-car c-notes)) (stream-cons (noflag) (loop (stream-cdr c-notes) c-curve))]
     [(eq? name 'freq)
      (loop
       c-notes
       (let [[tone (cadr (stream-car c-notes))]]
        (cond
         [(vector? tone) (constant-line (curve-length 'freq c-notes) (notevector->freq tone (cdr (assq 'scales head))))]
         [(pair? tone)
          (curve
           (map (lambda (x) (if (vector? x) (notevector->freq x (cdr (assq 'scales head))) x)) tone)
           expt-proc
           (curve-length 'freq c-notes))]
         [else (stream 0)])))]
     [else
      (let [[flagpair (assv name (stream-car c-notes))]]
       (if flagpair
        (loop
         c-notes
         (cond
          [(pair? (cdr flagpair)) (curve (cdr flagpair) linear-proc (curve-length (car flagpair) c-notes))]
          [(number? (cdr flagpair)) (constant-line (curve-length (car flagpair) c-notes) (cdr flagpair))]
          [else (stream (cdr flagpair))]))
        (stream-cons (noflag) (loop (stream-cdr c-notes) c-curve))))])))))
