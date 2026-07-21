#| env-macro.sld -- this file is part of SILVER: Source-fILter Version sound EmitteR 
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

(define-library (silver env macro)
 (import (scheme base) (only (silver source-filter) source mix) (srfi 1) (srfi 41))
 (export waves <flag>)
 (begin
  (define-syntax pipe
   (syntax-rules ()
    [(_ e) e]
    [(_ e p1 p2 ...) (pipe (p1 e) p2 ...)]))

  (define-syntax waves
   (syntax-rules ()
    [(_ [name chain ...] ...)
     (internal-waves () () () [name chain ...] ...)]))

  (define-syntax internal-waves
   (syntax-rules ()
    [(_ (f ...) (c ...) (done ...)) (let [[clist (list c ...)]]
     (cons (delete-duplicates (list 'freq f ...) eqv?)
           (lambda (name flags sr)
            (letrec [[source (source (assq 'freq flags) sr)]
                     [@@dummy-mod@@ (lambda (ignored) (stream-of 0))]
                     done ...]
             ((assv name clist) flags)))))]
    [(internal-waves (f ...) (c ...) (done ...) [name chain ...] other ...)
     (let [[prog (lambda (flags)
                  (pipe
                   (replace-c flags chain) ...
                   (replace-c flags (mix (<flag> #\v) @@dummy-mod@@))))]]
      (internal-waves
       (f ... (findf chain) ...)
       (c ... (cons name prog))
       ([name prog] done ...)
       other ...))]))

  (define-syntax findf
   (syntax-rules (<flag>)
    [(findf (l ...)  (prev ... (<flag> x) post ...))
     (findf (x l ...) (prev ... done post ...))]
    [(findf l done) l]))

  (define-syntax replace-c
   (syntax-rules (<flag>)
    [(replace-c flags (prev ... (<flag> x) post ...))
     (replace-c flags (prev ... (assv x flags) post ...))]
    [(_ prev ... (e ...) post ...) (syntax-error "Argument should not contain lists")]
    [(replace-c flags res) res]))))
