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
      (let ([xml_hash (make-hash)]
            [sym_hash (make-hash)])

        (map (lambda (sym) (hash-set! sym_hash sym #f)))

        (let loop-node ([xml_list (list (xml->xexpr (document-element (read-xml (current-input-port)))))])
          (when (not (null? xml_list))
                (let* ([node (car xml_list)]
                       [prefix (car node)]
                       [attr_list (cadr node)]
                       [content_list (cddr node)])
                  
                  (printf "prefix:~a\n" prefix)
                  (printf "attr_list:~a\n" attr_list)
                  (printf "content_list:~a\n\n\n" content_list)

                  (let loop-attr ([attrs attr_list])
                    (when (not (null? attrs))
                          (let ([item (string->symbol (string-append (symbol->string prefix) "." (symbol->string (caar attrs)))) (cadar attr
                          (hash-set! xml_hash (string->symbol (string-append (symbol->string prefix) "." (symbol->string (caar attrs)))) (cadar attrs))
                          (loop-attr (cdr attrs))))

                  (loop-node content_list)
                  
                  (loop-node (cdr xml_list)))))

        xml_hash))))
