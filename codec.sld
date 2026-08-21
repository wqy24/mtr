#| codec.sld -- this file is part of CHARIOT: Curves, Hackability And Restriction-less Instrument Oriented Tracker
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

(define-library (chariot codec)
 (import (except (scheme base) define) (only (srfi 1) every concatenate) (only (srfi 219) define) (chariot config) (scheme write))
 (export codec)
 (begin
  (define ((scale byte-depth) data)
   (truncate (* data (- (expt 2 (- (* 8 byte-depth) 1)) 1))))

  (define ((unsign byte-depth) data)
   (+ data (expt 2 (- (* 8 byte-depth) 1))))

  (define ((explode byte-depth big-endian) data)
   (let loop [[udata (if (negative? data) (+ data (expt 2 (* 8 byte-depth))) data)]
              [res '()] [rbdepth byte-depth]]
    (if (zero? rbdepth)
     (if big-endian res (reverse res))
     (loop
      (quotient udata 256)
      (cons (remainder udata 256) res)
      (- rbdepth 1)))))

  (define (codec data)
   (unless (every (lambda (d) (<= -1 d 1)) data) (error "Data out of range" data))
   (unless (integer? byte-depth) "Byte depth must be integer" byte-depth)
    (let* [[sdata (map (scale (byte-depth)) data)]
           [idata (if (signed) sdata (map (unsign byte-depth) sdata))]
           [exploded (map (explode (byte-depth) (big-endian)) idata)]]
     (apply bytevector (concatenate exploded))))))
