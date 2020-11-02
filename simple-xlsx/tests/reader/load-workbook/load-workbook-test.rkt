#lang racket

(require rackunit/text-ui)

(require rackunit "../../../reader.rkt")

(require racket/runtime-path)
(define-runtime-path workbook_file "workbook.xml")

(define test-load-workbook
  (test-suite
   "test-load-workbook"
   
   (test-case
    "test-load-workbook"

    (let ([xml_map (load-workbook workbook_file)])
      (check-equal? (hash-count xml_map) 17)

      (check-equal? (hash-ref xml_map "fileVersion.appName") "xl")
      (check-equal? (hash-ref xml_map "fileVersion.rupBuild") "4505")

      (check-equal? (hash-ref xml_map "sheet.count") 10)

      (check-equal? (hash-ref xml_map "sheet1.name") "DataSheet")
      (check-equal? (hash-ref xml_map "sheet1.sheetId") "1")
      (check-equal? (hash-ref xml_map "sheet1.r:id") "rId1")

      (check-equal? (hash-ref xml_map "sheet5.name") "LineChart2")
      (check-equal? (hash-ref xml_map "sheet5.sheetId") "5")
      (check-equal? (hash-ref xml_map "sheet5.r:id") "rId5")

      (check-equal? (hash-ref xml_map "sheet10.name") "PieSheet3D")
      (check-equal? (hash-ref xml_map "sheet10.sheetId") "10")
      (check-equal? (hash-ref xml_map "sheet10.r:id") "rId10")

      ))))

(run-tests test-load-workbook)
