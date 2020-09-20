#lang racket

(require rackunit/text-ui)

(require rackunit "../../../reader.rkt")

(require racket/runtime-path)
(define-runtime-path sharedStrings_file "sharedStrings.xml")

(define test-get-shared-strings
  (test-suite
   "test-get-shared-strings"
   
   (test-case
    "test-get-shared-strings"

    (let ([shared_strings_map (get-shared-strings sharedStrings_file)])
      (check-equal? (hash-count shared_strings_map) 17)
      (check-equal? (hash-ref shared_strings_map 1) "201601")
      (check-equal? (hash-ref shared_strings_map 16) "month/brand")
      ))))

(run-tests test-get-shared-strings)
