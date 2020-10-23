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

    (let* ([xml_hash (load-xml xml_file '(sheet))])
      (printf "~a\n" xml_hash)

      (check-equal? (hash-count xml_hash) 14)
      (check-equal? (hash-count (hash-ref xml_hash 'sheets)) 10)
      (check-equal? (hash-ref xml_hash 'workbook.xmlns) "http://schemas.openxmlformats.org/spreadsheetml/2006/main")
      (check-equal? (hash-ref xml_hash 'fileVersion.appName) "xl")
      (check-equal? (hash-ref xml_hash 'workbookView.xWindow) "0")
      (check-equal? (hash-ref xml_hash 'calcPr.calcId) "124519")

      (let ([sheets (hash-ref xml_hash 'sheets)])
        (check-equal? (hash-ref (list-ref sheets 0) 'name) "DataSheet")
        (check-equal? (hash-ref (list-ref sheets 0) 'sheetId) "1")
        (check-equal? (hash-ref (list-ref sheets 0) 'r:id) "rId1")

        (check-equal? (hash-ref (list-ref sheets 9) 'name) "PieChart")
        (check-equal? (hash-ref (list-ref sheets 9) 'sheetId) "10")
        (check-equal? (hash-ref (list-ref sheets 9) 'r:id) "rId10")
        )
      )

    )
    
  ))

(run-tests test-xml)
