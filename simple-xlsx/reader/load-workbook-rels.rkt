#lang racket

(provide (contract-out
          [load-workbook-rels (-> path-string? hash?)]
          ))

(require "../lib/xml.rkt")

(define (load-workbook-rels workbook_relation_file)
  (let ([xml_hash (load-xml-hash workbook_relation_file '(Relationship))]
        [data_hash (make-hash)])
    (let ([relation_ship_count (hash-ref xml_hash "Relationship.count")])
      (let loop ([loop_count 1])
        (when (<= loop_count relation_ship_count)
              (let* (
                     [relation_ship_id (hash-ref xml_hash (format "Relationship~a.Id" loop_count))]
                     [relation_ship_target (hash-ref xml_hash (format "Relationship~a.Target" loop_count))]
                     )
                (hash-set! data_hash relation_ship_id relation_ship_target)
                (loop (add1 loop_count))))))
    data_hash))
