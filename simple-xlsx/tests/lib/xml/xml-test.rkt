#lang racket

(require rackunit/text-ui)
(require racket/date)

(require racket/runtime-path)
(define-runtime-path xml_file "workbook.xml")

(require rackunit "../../../lib/xml.rkt")

(define test-xml
  (test-suite
   "test-xml"

   (test-case
    "test-load-xml-list"

    (let ([xml_hash (load-xml xml_file)])
      (printf "~a\n" xml_hash)
      )

    )
    
  ))

(run-tests test-xml)
