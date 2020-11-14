#lang racket

(require rackunit/text-ui)

(require rackunit "../../../reader/load-shared-strings.rkt")

(require racket/runtime-path)
(define-runtime-path sharedStrings_file "sharedStrings.xml")

(define test-load-shared-strings
  (test-suite
   "test-load-shared-strings"
   
   (test-case
    "test-load-shared-strings"

    (let ([shared_strings_map (load-shared-strings sharedStrings_file)])
      (check-equal? (hash-count shared_strings_map) 17)
      (check-equal? (hash-ref shared_strings_map 1) "")
      (check-equal? (hash-ref shared_strings_map 2) "201601")
      (check-equal? (hash-ref shared_strings_map 17) "month/brand")
      ))))

(run-tests test-load-shared-strings)
