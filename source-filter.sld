#| source-filter.sld -- this file is part of SILVER: Source-fILter Version sound EmitteR 
 | Copyright (C) 2026 wqy24
 |
 | SILVER is free software: you can redistribute it and/or modify
 | it under the terms of the GNU General Public License as published by
 | the Free Software Foundation, either version 3 of the License, or
 | (at your option) any later version.
 |
 | SILVER is distributed in the hope that it will be useful,
 | but WITHOUT ANY WARRANTY; without even the implied warranty of
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 | GNU General Public License for more details.
 |
 | You should have received a copy of the GNU General Public License
 | along with this program. If not, see <https://www.gnu.org/licenses/>.
 |#

(define-library (silver source-filter)
 (import (except (scheme base) define) (only (srfi 219) define) (wqy24 math) (srfi 41) (srfi* 41))
 (export source mulfreq integrator resonator differentiator mix allpass)
 (begin
  (define (with-default d s)
   (stream-map (lambda (x) (if (procedure? x) d x)) s))

  (define (source freq-curve sample-rate)
   (define step (/ 1 sample-rate))
   (let loop [[phase 0] [f0 (with-default 0 freq-curve)]]
    (if (stream-null? f0)
     stream-null
     (let* [[c-f0 (stream-car f0)] [new-phase (+ phase (* f0 step))]]
      (if (> new-phase 1)
       (stream-cons 1.0 (loop (- new-phase 1) (stream-cdr f0)))
       (stream-cons 0.0 (loop new-phase) (stream-cdr f0)))))))

  (define ((mulfreq x) input)
   (define index (stream-from 0))
   (define prd (period input))
   (define phase
    (stream-cdr
     (stream-scan*
      (lambda (ph0 impulse p)
       (if (zero? impulse) (fmod (+ ph0 (/ x p)) 1) 0))
      0
      input prd)))
   (stream-map
    (lambda (ph impulse p)
     (if (and (zero? impulse) (< (+ ph (/ x p)) 1))
      0 1))
    phase input prd))

  (define ((integrator) input)
   (stream-cdr
    (stream-scan + 0 input)))

  (define (period input)
   (define index (stream-from 0))
   (define t-stream
    (stream-scan*
     (lambda (t0 n impulse)
      (+ (* t0 (- 1 impulse))
         (* n impulse)))
     -1
     index input))
   (stream-cdr
    (stream-scan*
     (lambda (p0 n t0 impulse)
      (+ (* p0 (- 1 impulse))
         (* (- n t0) impulse))
      1
      index t-stream input))))

  (define ((resonator f0-source) input)
   (define r 0.99999)
   (define pi*2 (* 2 (acos -1)))
   (define res
    (stream-cons* 0 0
     (stream-map
      (lambda (y1 y2 p sample)
       (+ (* 2 (cos (/ pi*2 p)) r y1)
          (* -1 r r y2)
          sample))
      (stream-cdr res) res (period f0-source) input)))
   (stream-cdr res))))

  (define ((differentiator) input)
   (stream-map - input (stream-cons 0 input)))

  (define ((mix rto modulate) input)
   (define ratio (with-default 0 rto))
   (stream-map
    (lambda (dry wet r)
     (+ (* r wet)
        (* (- 1 r) dry)))
    input (modulate input) ratio))

  (define ((allpass level) input)
   (define omin 0.000488828125)
   (define omax (- (acos -1) omin))
   (define g-stream
    (stream-map
     (lambda (x) (cos (* (acos -1) 0.5 (expt (/ omax omin) x))))
    (with-default 0 level)))
   (stream-cdr
    (stream-scan*
     (lambda (y0 x x0 g)
      (+ (* -1 g x)
         x0
         (* g y0)))
     0
     input (cons 0 input) g-stream)))))
