#| render.sld -- this file is part of CHARIOT: Curves, Hackability And Restriction-less Instrument Oriented Tracker
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

(define-library (chariot render)
 (import (scheme eval) (scheme base) (chariot read) (chariot config) (srfi 41) (srfi 1) (wqy24 debug))
 (export render-channel merge-channels)
 (begin
  (define (render-channel head notes)
   (define engine (cdr (assq 'engine head)))
   (define inst (cdr (assq 'inst head)))
   (define engine-desc (cdr (assoc engine (cdr (assq 'inst-conf head)))))
   (define env (environment (car engine-desc) '(only (scheme base) quote)))
   (define config (cdr engine-desc))
   (define-values [flags renderer] (eval `(renderer ',config) env))
   (renderer inst (map (lambda (f) (cons f (get-curve f notes head))) flags) (sample-rate)))

  (define (merge-channels notes vols)
   (when (> (fold + 0 vols) 1)
    (error "Sum of vols more than 1" vols))
   (apply stream-map +
    (map (lambda (n v) (stream-map (lambda (x) (* x v)) n)) notes vols)))))
