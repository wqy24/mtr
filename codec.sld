(define-library (chariot codec)
 (import (except (scheme base) define) (srfi 1) (srfi 197) (only (srfi 219) define))
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
      (quotient udata 8)
      (cons (remainder udata 8) res)
      (- rbdepth 1)))))

  (define (codec signed byte-depth big-endian data)
   (unless (every (lambda (d) (<= -1 d 1)) data) (error "Data out of range" data))
   (unless (integer? byte-depth) "Byte depth must be integer" byte-depth)
   (chain data
    (map (scale byte-depth) _)
    (if signed _ (map (unsign byte-depth) _))
    (map (explode byte-depth big-endian) _)
    (concatenate _)
    (apply bytevector _)))))
