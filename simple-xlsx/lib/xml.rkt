#lang racket

(require xml)
(require xml/xexpr)

(provide (contract-out
          [load-xml-hash (-> path-string? (listof symbol?) hash?)]
          ))

(define (load-xml-hash xml sym_list)
  (with-input-from-file
      xml
    (lambda ()
      (let ([xml_hash (make-hash)]
            [sym_hash (make-hash)]
            )

        (map (lambda (sym) (hash-set! sym_hash sym #f)) sym_list)

        (let loop-node ([xml_list (list (xml->xexpr (document-element (read-xml (current-input-port)))))])
          (when (not (null? xml_list))
                (let* ([node (car xml_list)]
                       [prefix (car node)]
                       [attr_list (cadr node)]
                       [content_list (cddr node)])
                  
                  (let loop-attr ([attrs attr_list])
                    (when (not (null? attrs))
                          (when (hash-has-key? sym_hash prefix)
                                (let ([count_sym (format "~a.count" prefix)])
                                  (hash-set! xml_hash count_sym (add1 (hash-ref xml_hash count_sym 0)))
                                  (set! prefix (format "~a~a" prefix (hash-ref xml_hash count_sym)))))
                          (hash-set! xml_hash (format "~a.~a" prefix (caar attrs)) (cadar attrs))
                          (loop-attr (cdr attrs))))

                  (loop-node content_list)
                  
                  (loop-node (cdr xml_list)))))

        xml_hash))))
