#lang racket

(require rackunit/text-ui)
(require racket/date)

(require racket/runtime-path)
(define-runtime-path workbook_xml_file "workbook.xml")
(define-runtime-path sharedStrings_xml_file "sharedStrings.xml")

(require rackunit "../../../lib/xml.rkt")

(define test-xml
  (test-suite
   "test-workbook"

   (test-case
    "test-workbook"

    (let ([xml_hash (load-xml-hash workbook_xml_file '(sheet))])
      (check-equal? (hash-count xml_hash) 58)
      (check-equal? (hash-ref xml_hash "workbook.xmlns") "http://schemas.openxmlformats.org/spreadsheetml/2006/main")
      (check-equal? (hash-ref xml_hash "fileVersion.appName") "xl")
      (check-equal? (hash-ref xml_hash "workbookView.xWindow") "0")
      (check-equal? (hash-ref xml_hash "calcPr.calcId") "124519")

      (check-equal? (hash-ref xml_hash "sheet.count") 10)

      (check-equal? (hash-ref xml_hash "sheet1.name") "DataSheet")
      (check-equal? (hash-ref xml_hash "sheet1.sheetId") "1")
      (check-equal? (hash-ref xml_hash "sheet1.r:id") "rId1")

      (check-equal? (hash-ref xml_hash "sheet10.name") "PieChart3D")
      (check-equal? (hash-ref xml_hash "sheet10.sheetId") "10")
      (check-equal? (hash-ref xml_hash "sheet10.r:id") "rId10")
      )

    )

   (test-case
    "test-shared-string"

    (let ([xml_hash (load-xml-hash sharedStrings_xml_file '(t phoneticPr))])
      (check-equal? (hash-count xml_hash) 73)
      (check-equal? (hash-ref xml_hash "sst.count") "17")
      (check-equal? (hash-ref xml_hash "sst.uniqueCount") "17")

      (check-equal? (hash-ref xml_hash "t.count") 17)
      (check-equal? (hash-ref xml_hash "phoneticPr.count") 17)

      (check-equal? (hash-ref xml_hash "t1") "")
      (check-equal? (hash-ref xml_hash "t2") "201601")
      (check-equal? (hash-ref xml_hash "t10") "Center")
      (check-equal? (hash-ref xml_hash "t17") "month/brand")
      )
    )
    
  ))

(run-tests test-xml)
