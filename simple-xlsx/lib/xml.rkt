#lang racket

(require xml)
(require xml/xexpr)

(provide (contract-out
          [load-xml (-> path-string? (listof symbol?) hash?)]
          ))

(define (load-xml xml sym_list)
  (with-input-from-file
      xml
    (lambda ()
      (let ([xml_list (xml->xexpr (document-element (read-xml (current-input-port))))])
        (make-hash)))))

(define (get-attr-hash node_xml)
  (let ([attr_hash (make-hash)])
    (let loop-attr ([attrs (cadr node_xml)])
      (when (not (null? attrs))
            (hash-set! attr_hash (caar attrs) (cadar attrs))
            (loop-attr (cdr attrs))))
    attr_hash))

