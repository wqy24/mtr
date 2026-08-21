#| envelope.sld -- this file is part of CHARIOT: Curves, Hackability And Restriction-less Instrument Oriented Tracker
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

(define-library (chariot envelope)
 (import (scheme base) (wqy24 vlws) (wqy24 debug) (chariot inst-utils))
 (export (rename envelope apply))
 (begin
  (define (envelope svol sdata)
   (stream-map
    (lambda (data vol)
     (* data (expt 10 (if (positive? vol) 0 vol))))
    sdata (with-default svol 0)))))
