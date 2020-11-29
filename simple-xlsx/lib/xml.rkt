#lang racket

(require xml)
(require xml/xexpr)

(provide (contract-out
          [load-xml-hash (-> path-string? (listof symbol?) hash?)]
          ))

(define (load-xml-hash xml sym_list)
  (call-with-input-file
   xml
   (lambda (origin_port)
     (call-with-input-string
      (regexp-replace* #rx"> *<"
                       (regexp-replace* #rx"\n|\r" (port->string origin_port) "")
                       "><")
      (lambda (filtered_port)
        (let ([xml_hash (make-hash)]
              [sym_hash (make-hash)]
              )

          (map (lambda (sym) (hash-set! sym_hash sym #f)) sym_list)

          (let loop-node ([ancester_prefix ""]
                          [xml_list (list (xml->xexpr (document-element (read-xml filtered_port))))])
            
            (printf "~a\n" xml_list)

            (when (not (null? xml_list))
                  (if (not (list? (car xml_list)))
                      (if (hash-has-key? sym_hash ancester_prefix)
                          (let ([count_sym (format "~a.count" ancester_prefix)])
                            (hash-set! xml_hash count_sym (add1 (hash-ref xml_hash count_sym 0)))
                            (hash-set! xml_hash (format "~a~a" ancester_prefix (hash-ref xml_hash count_sym)) (car xml_list)))
                          (hash-set! xml_hash ancester_prefix (car xml_list)))
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

                        (if (null? content_list)
                            (loop-node prefix '(""))
                            (loop-node prefix content_list))
                        
                        (loop-node prefix (cdr xml_list))))))
          xml_hash))))))
