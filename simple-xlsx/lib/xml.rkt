#lang racket

(require xml)
(require xml/xexpr)
(require xml/path)

(provide (contract-out
          [load-xml (-> path-string? hash?)]
          ))

(define (load-xml xml)
  (with-input-from-file
      xml
    (lambda ()
      (let ([result_hash (make-hash)])
        (let loop-node ([nodes (xml->xexpr (document-element (read-xml (current-input-port))))]
                        [index_str ""])

          (printf "nodes:~a\n" nodes)
          
          (printf "index_str:~a\n" index_str)
          
          (printf "result_hash:~a\n" result_hash)
          
          (when (not (null? nodes))
                (let ([node_name (car nodes)])
                  (printf "node_name:~a\n" node_name)
                  (let loop-attr ([attrs (cadr nodes)])
                    (printf "attrs:~a\n" attrs)
                    (when (not (null? attrs))
                          (hash-set! result_hash (string-append (symbol->string node_name) index_str "." (symbol->string (caar attrs))) (cadar attrs))
                          (loop-attr (cdr attrs))))

                  (hash-set! result_hash (string-append (symbol->string node_name) ".count") (length (cddr nodes)))
                  
                  (let ([contents (cddr nodes)])
                    (printf "contents:~a\n" contents)
                    (when (not (null? contents))
                          (if (> (length contents) 1)
                              (let loop-multi-content ([_contents contents]
                                                       [index 0])

                                (when (not (null? _contents))
                                      (loop-node (car _contents) (number->string index))
                          
                                      (loop-multi-content (cdr _contents) (add1 index))))
                              (loop-node (car contents) "")))))))
        result_hash))))

                  
                
                
