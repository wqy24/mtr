#| curves.sld -- this file is part of CHARIOT: Curves, Hackability And Restriction-less Instrument Oriented Tracker
 | Copyright (C) 2026 wqy24
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

(define-library (chariot curves)
 (import (scheme base) (scheme lazy) (srfi 1) (srfi 41) (wqy24 assert) (wqy24 math))
 (export bezier constant-line)
 (begin
  (define (curvepoint-deriv1 p0 p1 p2 p3 p4 p5)
   (let [[c4 (+ (* p5 5)
                (* p4 -25)
                (* p3 50)
                (* p2 -50)
                (* p1 25)
                (* p0 -5))]
         [c3 (+ (* p4 20)
                (* p3 -80)
                (* p2 120)
                (* p1 -80)
                (* p0 20))]
         [c2 (+ (* p3 30)
                (* p2 -90)
                (* p1 90)
                (* p0 -30))]
         [c1 (+ (* p2 20)
                (* p1 -40)
                (* p0 20))]
         [c0 (+ (* p1 5)
                (* p0 -5))]]
    (lambda (t)
     (let* [[t^2 (* t t)]
            [t^3 (* t t^2)]
            [t^4 (* t t^3)]]
      (+ (* c4 t^4)
         (* c3 t^3)
         (* c2 t^2)
         (* c1 t)
         c0)))))

  (define (extremum-deriv1 p0 p1 p2 p3 p4 p5) ; (zero? x) of deriv2, uses Shengjin algorithm
   (let [[a (+ (* p5 20)
               (* p4 -100)
               (* p3 200)
               (* p2 -200)
               (* p1 100)
               (* p0 -20))]
         [b (+ (* p4 60)
               (* p3 -240)
               (* p2 360)
               (* p1 -240)
               (* p0 60))]
         [c (+ (* p3 60)
               (* p2 -180)
               (* p1 180)
               (* p0 -60))]
         [d (+ (* p2 20)
               (* p1 -40)
               (* p0 20))]]
    (call-with-values (lambda () (shengjin a b c d)) (lambda x x))))

  (define (curvepoint p0 p1 p2 p3 p4 p5) ; Five-order bezier curvepoint (normalized)
   (let [[c5 (+ p5
                (* p4 -5)
                (* p3 10)
                (* p2 -10)
                (* p1 5)
                (- p0))]
         [c4 (+ (* p4 5)
                (* p3 -20)
                (* p2 30)
                (* p1 -20)
                (* p0 5))]
         [c3 (+ (* p3 10)
                (* p2 -30)
                (* p1 30)
                (* p0 -10))]
         [c2 (+ (* p2 10)
                (* p1 -20)
                (* p0 10))]
         [c1 (+ (* p1 5)
                (* p0 -5))]]
    (lambda (t)
     (let* [[t^2 (* t t)]
            [t^3 (* t t^2)]
            [t^4 (* t t^3)]
            [t^5 (* t t^4)]]
      (+ (* c5 t^5)
         (* c4 t^4)
         (* c3 t^3)
         (* c2 t^2)
         (* c1 t)
         p0)))))

  (define (validate . ps)
   (assert ps
    (lambda (ps)
     (apply
      (lambda (p0 p1 p2 p3 p4 p5)
       (let [[deriv1 (curvepoint-deriv1 p0 p1 p2 p3 p4 p5)]]
        (and
         (positive? (deriv1 0))
         (positive? (deriv1 1))
         (every
          (lambda (item)
           (positive? (deriv1 item)))
          (extremum-deriv1 p0 p1 p2 p3 p4 p5))))) ps)) "Bad curve"))

  (define (bezier divisions p0 p1 p2 p3 p4 p5) ; Returns a lazy list
   (let-values [[[x0 y0] (car+cdr p0)]
                [[x1 y1] (car+cdr p1)]
                [[x2 y2] (car+cdr p2)]
                [[x3 y3] (car+cdr p3)]
                [[x4 y4] (car+cdr p4)]
                [[x5 y5] (car+cdr p5)]]
    (when (= y0 y5) (error "Curve not appliable" (list p0 p1 p2 p3 p4 p5)))
    (when (>= x0 x5) (error "You can't go back through the time!" (list p0 p1 p2 p3 p4 p5))) ; Should never occur
    (validate x0 x1 x2 x3 x4 x5)
    (let* [[step (/ (- x5 x0) (- divisions 1))]
           [epsilon (/ step 16)]
           [point-x (curvepoint x0 x1 x2 x3 x4 x5)]
           [point-y (curvepoint y0 y1 y2 y3 y4 y5)]
           [deriv1-x (curvepoint-deriv1 x0 x1 x2 x3 x4 x5)]]
     (let again [[target-x x0] [t0 0] [samples divisions]]
      (if (positive? samples)
       (let*
        [[t
          (let iter [[curt t0]]
           (if (< (abs (- target-x (point-x curt))) epsilon)
            curt
            (iter (- curt (/ (- (point-x curt) target-x) (deriv1-x curt))))))]]
        (stream-cons
         (point-y t)
         (again (+ target-x step) t (- samples 1))))
       stream-null)))))

  (define (constant-line len val)
   (if (zero? len)
    stream-null
    (stream-cons val (constant-line (- len 1) val))))))
