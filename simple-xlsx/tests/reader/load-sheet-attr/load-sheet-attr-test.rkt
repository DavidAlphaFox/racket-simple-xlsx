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

    (let-values ([(_sheet_id_list _sheet_id_name_map _sheet_name_id_map)
                  (load-sheet-attr workbook_file)])
      
  ))

(run-tests test-load-sheet-attr)
