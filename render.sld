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
 (import (scheme eval) (scheme base) (chariot read) (chariot config) (srfi 1))
 (export render)
 (begin
  (define (render-channel head notes)
   (define inst-desc (cdr (assoc 'inst head)))
   (define env (environment (first inst-desc))
   (define config-head (second inst-desc))
   (define name (third inst-desc))
   (define config (cdr (assoc config-head head)))
   (define-values [flags renderer] (car+cdr (eval (list 'renderer config) env)))
   (renderer name flags SAMPLE-RATE))))
