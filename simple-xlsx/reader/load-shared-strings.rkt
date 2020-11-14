#lang racket

(provide (contract-out
          [load-shared-strings (-> path-string? hash?)]
          ))

(require "../lib/xml.rkt")

(define (load-shared-strings shared_string_file)
  (let ([xml_hash (load-xml-hash shared_string_file '(t phoneticPr))]
        [shared_hash (make-hash)])

    (let loop ([loop_count 1])
      (when (<= loop_count (hash-ref xml_hash "t.count"))
            (let ([t (hash-ref xml_hash (format "t~a" loop_count))])
              (hash-set! shared_hash
                         loop_count
                         (cond
                          [(string? t)
                           t]
                          [(integer? t)
                           (string (integer->char t))]
                          [else
                           ""])))
            (loop (add1 loop_count))))
    shared_hash))
