#lang racket

(require xml)
(require xml/xexpr)
(require xml/path)

(provide (contract-out
          [load-xml (-> path-string? xexpr?)]
          [get-xml-list (-> symbol? xexpr? list?)]
          [get-attr-hash (-> list? hash?)]
          ))

(define (load-xml xml)
  (with-input-from-file
      xml
    (lambda ()
      (xml->xexpr (document-element (read-xml (current-input-port)))))))

(define (get-xml-list node_sym xml_xpr)
  (se-path*/list (list node_sym) xml_xpr))


(define (get-attr-hash node_xml)
  (let ([attr_hash (make-hash)])
    (let loop-attr ([attrs (cadr nodes)])
      (when (not (null? attrs))
            (hash-set! attr_hash (caar attrs) (cadar attrs))
            (loop-attr (cdr attrs))))
    attr_hash))

