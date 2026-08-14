#| silver.sld -- this file is part of SILVER: SIne Layers to VariablE timbRes 
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

(define-library (silver)
 (import (scheme base) (srfi 41) (srfi 1))
 (export renderer)
 (begin
  (define (renderer config)
   (values '(freq)
    (lambda (name flags sr)
     (let [[inst (cdr (assv name config))]
           [freq (cdr (assq 'freq flags))]]
      (stream-map
       (lambda (f i)
        (apply +
         (map
          (lambda (x)
           (let [[a (car x)] [ph (cdr x)]]
            (cos (+ ph (* 2 (acos -1) (/ f sr) i)))))
          inst (iota (length inst) 1 1))))
       freq (stream-from 0))))))))
