#lang racket

(provide (contract-out
          [load-sheet-attr (-> path-string? (values (listof string?) hash? hash? hash?))]
          [get-shared-strings (-> path-string? hash?)]
          [get-rid-rel-map (-> path-string? hash?)]
          [with-input-from-xlsx-file (-> path-string? (-> (is-a?/c read-xlsx%) any) any)]
          [get-sheet-names (-> (is-a?/c read-xlsx%) list?)]
          [get-cell-value (-> string? (is-a?/c read-xlsx%) any)]
          [get-cell-formula (-> string? (is-a?/c read-xlsx%) string?)]
          [get-sheet-dimension (-> (is-a?/c read-xlsx%) pair?)]
          [load-sheet (-> string? (is-a?/c read-xlsx%) void?)]
          [load-sheet-ref (-> exact-nonnegative-integer? (is-a?/c read-xlsx%) void?)]
          [get-sheet-rows (-> (is-a?/c read-xlsx%) list?)]
          [sheet-name-rows (-> path-string? string? list?)]
          [sheet-ref-rows (-> path-string? exact-nonnegative-integer? list?)]
          ))

(require xml)
(require file/unzip)

(require "xlsx/xlsx.rkt")
(require "lib/lib.rkt")
(require "lib/xml.rkt")
(require "xlsx/range-lib.rkt")

(define (with-input-from-xlsx-file xlsx_file user_proc)
  (call-with-unzip
   xlsx_file
   (lambda (tmp_dir)
     (let (
           [sheet_id_list #f]
           [sheet_id_name_map #f]
           [sheet_name_id_map #f]
           [sheet_id_rid_map #f]
           [shared_strings_map #f]
           [sheet_rid_rel_map #f]
           [sheets '()]
           [fomula_map (make-hash)]
           [data_type_map (make-hash)]
           [xlsx_obj #f])

       (let-values ([(_sheet_id_list _sheet_id_name_map _sheet_name_id_map _sheet_id_rid_map)
                     (load-sheet-attr (build-path tmp_dir "xl" "workbook.xml"))])
         (set! sheet_id_list _sheet_id_list)
         (set! sheet_id_name_map _sheet_id_name_map)
         (set! sheet_name_id_map _sheet_name_id_map)
         (set! sheet_id_rid_map _sheet_id_rid_map)
         )

       (set! shared_strings_map (get-shared-strings (build-path tmp_dir "xl" "sharedStrings.xml")))

       (set! sheet_rid_rel_map (get-rid-rel-map (build-path tmp_dir "xl" "_rels" "workbook.xml.rels")))
       
       (set! xlsx_obj
             (new read-xlsx%
                  (xlsx_dir tmp_dir)
                  (shared_strings_map shared_strings_map)
                  (sheet_name_map sheet_name_id_map)
                  (relation_name_map sheet_id_relation_map)))
       (user_proc xlsx_obj)))))

(define (load-sheet-attr workbook_xml)
  (let ([xml_hash (load-xml-hash workbook_xml '(sheet))]
        [sheet_id_list '()]
        [sheet_id_name_map (make-hash)]
        [sheet_name_id_map (make-hash)]
        [sheet_id_rid_map (make-hash)]
        )
    (let loop ([loop_count 1])
      (when (<= loop_count (hash-ref xml_hash "sheet.count"))
            (let ([sheet_name (hash-ref xml_hash (format "sheet~a.name" loop_count))]
                  [sheet_id (hash-ref xml_hash (format "sheet~a.sheetId" loop_count))]
                  [rid (hash-ref xml_hash (format "sheet~a.r:id" loop_count))])
              (set! sheet_id_list `(,@sheet_id_list ,sheet_id))
              (hash-set! sheet_id_rid_map sheet_id rid)
              (hash-set! sheet_name_id_map sheet_name sheet_id)
              (hash-set! sheet_id_name_map sheet_id sheet_name))
            (loop (add1 loop_count))))
      (values
       sheet_id_list
       sheet_id_name_map
       sheet_name_id_map
       sheet_id_rid_map)))

(define (get-shared-strings shared_string_file)
  (let ([data_map (make-hash)])
    (when (file-exists? shared_string_file)
          (with-input-from-file
              shared_string_file
            (lambda ()
              (let ([xml_str (port->string (current-input-port))])
                (let loop2 ([split_items1 (regexp-split #rx"</si>" xml_str)]
                            [index 0])
                  (when (not (null? split_items1))
                        (let ([split_items2 (regexp-split #rx"<si>" (car split_items1))])
                          (when (> (length split_items2) 1)
                                (let* ([xml (xml->xexpr (document-element (read-xml (open-input-string (string-append "<si>" (second split_items2) "</si>")))))]
                                       [v_list (xml-get-list 't xml)]
                                       [ignore_rPh_map (make-hash)])
                                  
                                  (for-each
                                   (lambda (rPh_rec)
                                     (hash-set! ignore_rPh_map (third rPh_rec) ""))
                                   (xml-get-list 'rPh xml))
                                  
                                  (hash-set! data_map
                                             index
                                             (regexp-replace* 
                                              #rx" " 
                                              (foldr (lambda (a b) (string-append a b)) "" 
                                                     (filter 
                                                      (lambda (item) 
                                                        (not (hash-has-key? ignore_rPh_map item))) 
                                                      (map
                                                       (lambda (v)
                                                         (cond
                                                          [(string? v)
                                                           v]
                                                          [(integer? v)
                                                           (string (integer->char v))]
                                                          [else
                                                           ""]))
                                                       v_list)))
                                              " ")))))
                        (loop2 (cdr split_items1) (add1 index))))))))
    data_map))

(define (get-rid-rel-map workbook_relation_file)
  (let ([data_map (make-hash)])
    (with-input-from-file
        workbook_relation_file
      (lambda ()
        (let ([xml (xml->xexpr (document-element (read-xml (current-input-port))))])
          (let loop ([loop_list (xml-get-list 'Relationships xml)])
            (when (not (null? loop_list))
                  (if (not (list? (car loop_list)))
                      (loop (cdr loop_list))
                      (let ([attr_list (cadr (car loop_list))]
                            [sheet_name ""]
                            [sheet_id ""])
                        (for-each
                         (lambda (attr_pair)
                           (let ([name (car attr_pair)]
                                 [value (cadr attr_pair)])
                             (cond
                              [(equal? name 'Id)
                               (set! sheet_name value)]
                              [(equal? name 'Target)
                               (set! sheet_id value)])))
                         attr_list)
                        (hash-set! data_map sheet_name sheet_id)))
                  (loop (cdr loop_list)))))))
    data_map))

(define (get-sheet-names xlsx)
  (map
   (lambda (item)
     (car item))
   (sort #:key cdr (hash->list (get-field sheet_name_map xlsx)) string<?)))

(define (load-sheet sheet_name xlsx)
  (let ([data_map (make-hash)]
        [formula_map (make-hash)]
        [type_map (make-hash)]
        [dimension_col 0]
        [dimension ""]
        [rows #f]
        [data_sheet_file_name
         (build-path (get-field xlsx_dir xlsx) "xl" (hash-ref (get-field relation_name_map xlsx) (hash-ref (get-field sheet_name_map xlsx) sheet_name)))])
    
    (when (string=? (path->string (car (take-right (explode-path data_sheet_file_name) 2))) "worksheets")
          (let ([file_str (file->string data_sheet_file_name)])
            (set! rows
                  (let loop ([loop_list
                              (regexp-split #rx"<sheetData>|</sheetData>|<row" file_str)]
                             [result_list '()]
                             [index 1])
                    (if (not (null? loop_list))
                        (begin
                          (if (regexp-match #rx"</row>" (car loop_list))
                              (let ([row_index (second (regexp-match #rx" r=\"([0-9]+)\" " (car loop_list)))]
                                    [col_info (regexp-match* #rx" r=\"([A-Z]+)[0-9]+\" *" (car loop_list))])
                                (if (= (string->number row_index) index)
                                    (let ([row (xml->xexpr (document-element (read-xml (open-input-string (string-append "<row" (car loop_list))))))])
                                      (when (not (null? col_info))
                                            (let ([max_col_index (abc->number 
                                                                  (car (reverse 
                                                                        (map 
                                                                         (lambda (item)
                                                                           (second (regexp-match #rx"([A-Z]+)" item)))
                                                                         col_info))))])
                                              (when (> max_col_index dimension_col)
                                                    (set! dimension_col max_col_index))))
                                      (loop
                                       (cdr loop_list)
                                       (cons
                                        (xml->xexpr (document-element (read-xml (open-input-string (string-append "<row" (car loop_list)))))) 
                                        result_list)
                                       (add1 index)))
                                    (loop
                                     loop_list
                                     (cons null result_list)
                                     (add1 index))))
                              (loop (cdr loop_list) result_list index)))
                        (reverse result_list)))))
          
          (set-field! dimension xlsx (cons (length rows) dimension_col))
          
          (for-each
           (lambda (row_xml)
             (when (not (null? row_xml))
                   (for-each
                    (lambda (cell_item)
                      (when (list? cell_item)
                            (let ([first_item (car cell_item)])
                              (when (and (symbol? first_item) (equal? first_item 'c))
                                    (let ([para_part (second cell_item)]
                                          [para_r ""]
                                          [para_s ""]
                                          [para_t ""])
                                      (let loop ([para_list para_part])
                                        (when (not (null? para_list))
                                              (let* ([para (car para_list)]
                                                     [key (car para)]
                                                     [value (cadr para)])
                                                (cond
                                                 [(equal? key 'r)
                                                  (set! para_r value)]
                                                 [(equal? key 's)
                                                  (set! para_s value)]
                                                 [(equal? key 't)
                                                  (set! para_t value)]
                                                 ))
                                              (loop (cdr para_list))))
                                      (hash-set! type_map para_r (cons para_t para_s))

                                      (let loop-cell ([cell_list (cdr cell_item)])
                                        (when (not (null? cell_list))
                                              (cond 
                                               [(equal? (caar cell_list) 'v)
                                                (hash-set! data_map para_r (caddar cell_list))]
                                               [(equal? (caar cell_list) 'f)
                                                (hash-set! formula_map para_r (caddar cell_list))]
                                               )
                                              (loop-cell (cdr cell_list))))
                                      )))))
                    row_xml)))
           rows)
          )
    
    (list data_map formula_map type_map)))

(define (load-sheet-ref sheet_index xlsx)
  (load-sheet (list-ref (get-sheet-names xlsx) sheet_index) xlsx))

(define (get-cell-value item_name xlsx)
  (let ([sheet_map (get-field sheet_map xlsx)]
        [data_type_map (get-field data_type_map xlsx)]
        [shared_map (get-field shared_map xlsx)])
    (if (and
         (hash-has-key? sheet_map item_name)
         (not (null? (hash-ref data_type_map item_name))))
        (let* ([type (hash-ref data_type_map item_name)]
               [type_t (car type)]
               [type_s (cdr type)]
               [value (hash-ref sheet_map item_name)])
          (cond
           [(string=? type_t "s")
            (hash-ref shared_map value)]
           [(string=? type_t "n")
            (string->number value)]
           [(string=? type_t "")
            (string->number value)]))
        "")))

(define (get-cell-formula item_name xlsx)
  (let ([formula_map (get-field formula_map xlsx)])
    (if (hash-has-key? formula_map item_name)
        (hash-ref formula_map item_name)
        "")))

(define (get-sheet-dimension xlsx)
  (get-field dimension xlsx))

(define (get-sheet-rows xlsx)
  (let ([dimension null]
        [rows null]
        [cols null])
    
    (set! dimension (get-field dimension xlsx))
    
    (set! rows (car dimension))

    (set! cols (cdr dimension))

    (let loop ([row_index 1]
               [result_list '()])
      (if (<= row_index rows)
          (loop 
           (add1 row_index)
           (cons 
            (map
             (lambda (col_index)
               (get-cell-value (string-append (number->abc col_index) (number->string row_index)) xlsx))
             (number->list cols))
            result_list))
          (reverse result_list)))))

(define (sheet-name-rows xlsx_file_path sheet_name)
  (with-input-from-xlsx-file
   xlsx_file_path
   (lambda (xlsx)
     (load-sheet sheet_name xlsx)
     
     (get-sheet-rows xlsx))))

(define (sheet-ref-rows xlsx_file_path sheet_index)
  (with-input-from-xlsx-file
   xlsx_file_path
   (lambda (xlsx)
     (load-sheet-ref sheet_index xlsx)
     
     (get-sheet-rows xlsx))))

