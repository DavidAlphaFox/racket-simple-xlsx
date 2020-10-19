#lang racket

(require xml)
(require xml/xexpr)
(require xml/path)

(provide (contract-out
          [load-xml (-> path-string? xexpr?)]
          [get-xml-content (-> (listof symbol?) xexpr? any)]
          [get-xml-list (-> (listof symbol?) xexpr? list?)]
          [get-attr-hash (-> list? hash?)]
          ))

(define (load-xml xml)
  (with-input-from-file
      xml
    (lambda ()
      (xml->xexpr (document-element (read-xml (current-input-port)))))))

(define (get-xml-content sym_list xml_xpr)
  (se-path* sym_list xml_xpr))

(define (get-xml-list sym_list xml_xpr)
  (se-path*/list sym_list xml_xpr))

(define (get-attr-hash node_xml)
  (let ([attr_hash (make-hash)])
    (let loop-attr ([attrs (cadr node_xml)])
      (when (not (null? attrs))
            (hash-set! attr_hash (caar attrs) (cadar attrs))
            (loop-attr (cdr attrs))))
    attr_hash))

