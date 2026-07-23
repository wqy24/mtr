#| render.sld -- this file is part of SILVER: Source-fILter Version sound EmitteR
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

(define-library (silver render)
 (import (scheme base) (scheme eval))
 (export renderer)
 (begin
  (define (renderer config)
   (eval (cons 'waves config) (environment '(silver env macro) '(silver source-filter))))))
