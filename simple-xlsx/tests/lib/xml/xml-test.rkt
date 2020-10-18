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
    "test-get-xml-list"

    (let* ([xml_xpr (load-xml xml_file)]
           [sheet_list (get-xml-list 'sheets xml_xpr)]
           )
      (check-equal? (length sheet_list) 10)
      (printf "~a\n" (get-xml-content '(fileVersion #:appName) xml_xpr))
      )

    )
    
  ))

(run-tests test-xml)
