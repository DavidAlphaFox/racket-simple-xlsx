#lang racket

(require rackunit/text-ui)

(require rackunit "../../../reader.rkt")

(require racket/runtime-path)
(define-runtime-path workbook_file "workbook.xml")

(define test-load-sheet-attr
  (test-suite
   "test-load-sheet-attr"
   
   (test-case
    "test-load-sheet-attr"

    (let-values ([(_sheet_id_list _sheet_id_name_map _sheet_name_id_map _sheet_id_rid_map)
                  (load-sheet-attr workbook_file)])
      (check-equal? (length _sheet_id_list) 10)
      (check-equal? (list-ref _sheet_id_list 0) "1")
      (check-equal? (list-ref _sheet_id_list 9) "10")
      (check-equal? (hash-count _sheet_id_name_map) 10)
      (check-equal? (hash-ref _sheet_id_name_map "8") "BarChart3D")
      (check-equal? (hash-count _sheet_name_id_map) 10)
      (check-equal? (hash-ref _sheet_name_id_map "BarChart3D") "8")
      (check-equal? (hash-count _sheet_id_rid_map) 10)
      (check-equal? (hash-ref _sheet_id_rid_map "1") "rId1")
      ))))

(run-tests test-load-sheet-attr)
