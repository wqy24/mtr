#| chariot-engine.scm -- this file is part of CHARIOT: Curves, Hackability And Restriction-less Instrument Oriented Tracker
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

(import (scheme base) (scheme cxr) (scheme read) (wqy24 vlws) (chariot config) (chariot read) (chariot render) (chariot codec) (wqy24 debug))

(define module (read))

(define channels
 (let [[data (cdr (assq 'channels module))]]
  (map (lambda (d) (let ([head (append (car d) module)])
                     (cons head (get-notes (cdr d) head)))) data)))

(define output-conf (cond [(assq 'output module) => cdr] [else '()]))

(define-syntax init-param
 (syntax-rules ()
  [(_ conf [item ...] body ...)
   (let [[c conf]]
    (parameterize
     [[item (cond [(assq 'item c) => cdr] [else (item)])]
       ...]
     body ...))]))

(init-param output-conf #0=[sample-rate byte-depth big-endian signed]
 (let again [[command (read)]]
  (case (car command)
   [[play]
    (let* [[start-frm (cadr command)]
           [len (caddr command)]
           [chns (map (lambda (c) (cons (car c) (stream-drop (cdr c) start-frm))) channels)]
           [audio-stream
            (merge-channels
             (map (lambda (c) (render-channel (car c) (cdr c))) chns)
             (map (lambda (c) (cdr (assq 'volume (car c)))) chns))]]
     (write-bytevector
      (codec
       (stream->list
        (if (integer? len) (stream-take audio-stream len) audio-stream)))))
    (again (read))]
   [[tmp-set]
    (let [[p (assq (cadr command) module)]]
     (if p
      (set-cdr! p (caddr command))
      (set! module (cons (cons (cadr command) (caddr command)) module))))
    (again (read))]
   [[exit] 0])))
