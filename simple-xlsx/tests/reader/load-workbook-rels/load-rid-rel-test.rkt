#lang racket

(require rackunit/text-ui)

(require rackunit "../../../reader.rkt")

(require racket/runtime-path)
(define-runtime-path workbook_rels_file "workbook.xml.rels")

(define test-load-workbook-rels
  (test-suite
   "test-load-workbook-rels"
   
   (test-case
    "test-load-workbook-rels"

    (let ([data_map (load-workbook-rels workbook_rels_file)])
      (check-equal? (hash-count data_map) 13)

      (check-equal? (hash-ref data_map "rId1") "worksheets/sheet1.xml")
      (check-equal? (hash-ref data_map "rId4") "chartsheets/sheet1.xml")
      (check-equal? (hash-ref data_map "rId5") "chartsheets/sheet2.xml")
      (check-equal? (hash-ref data_map "rId10") "chartsheets/sheet7.xml")

      ))))

(run-tests test-load-workbook-rels)
