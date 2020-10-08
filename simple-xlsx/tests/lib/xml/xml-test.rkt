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
           [sheet_list (get-xml-list 'sheets xml_xpr)])
      (printf "~a\n" sheet_list)
      (check-equal? (length sheet_list) 10)
      )

    )
    
  ))

(run-tests test-xml)
