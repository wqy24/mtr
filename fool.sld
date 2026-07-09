#| fool.sld -- this file is part of CHARIOT: Curves, Hackability And Restriction-less Instrument Oriented Tracker
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

(define-library (chariot fool) ; Stand for "Frequency tOOLs"
 (import (scheme base) (scheme eval))
 (export notevector->freq)
 (begin
  (define (foolish-eval sexp)
   (eval sexp '(chariot fool env)))
  (define (notevector->freq v scales)
   (let* [[scale (cdr (assv (vector-ref v 2) scales))]
          [octave-rate (foolish-eval (cdr (assq 'octave-rate scale)))]
          [C-0 33/2]
          [note-ratio (foolish-eval (cdr (assoc (vector-ref v 0) scale)))]
          [octaves (foolish-eval (vector-ref v 1))]
          [octave-align (foolish-eval (cdr (assq 'octave-align scale)))]]
    (* C-0 note-ratio (expt 2 octave-align) (expt octave-rate (- octaves octave-align)))))))
