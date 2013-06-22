(defpackage text-tables
  (:use common-lisp)
  (:nicknames txt-tbls)
  (:export tbl-append print-table))

(in-package text-tables)


;; table -> (title #(row) #(row) ...)
;; title, row -> (column, column, ... column)

(defun tbl-append (tbl row)
  "appends row 'row' to the table 'tbl'"
  (declare (list tbl) 
           (array row))
  
  (let ((row-list (list row)))
    (if tbl
        (if (= (length (car tbl)) (length row))
            (append tbl row-list)
            (error "Row lengths doesn't match!"))
        row-list)))

(defun find-widest-item (tbl column-number)
  "finds max width in the column number 'column-number' in table 'tbl'"
  (loop for x in tbl maximize (length (aref x column-number))))

(defun print-table (tbl &optional (delimiter #\|))
  "prints table 'tbl' with delimiter 'delimiter' to the standard output"
  (flet ((print-row (row lengths)
           (loop for column across row
                 for column-length across lengths do
                (let* ((avail-ws (- column-length (length column)))
                       (ws-before (round (/ avail-ws 2)))
                       (ws-after (- avail-ws ws-before)))
                  (loop repeat ws-before do (write #\Space :escape nil))
                  (write column :escape nil)
                  (loop repeat ws-after do (write #\Space :escape nil))
                  (write delimiter :escape nil)))))

    (let* ((header (car tbl))
           (header-length (length header)))
      (if tbl
          (let ((lengths (make-array header-length :element-type 'integer)))
            (dotimes (i (length (car tbl)))
              ;; add whitespaces before and after: +2
              (setf (aref lengths i)
                    (+ (find-widest-item tbl i) 2)))
            
            ;; print header
            (print-row header lengths)
            (write #\Newline :escape nil)
            
            ;; print horizontal delimiter
            (let ((total-length (loop for x across lengths sum x)))
              (loop repeat total-length do (write #\_ :escape nil)))
            (write #\Newline :escape nil)
            
            ;; print rest table
            (loop for row in (cdr tbl) do
                 (print-row row lengths)
                 (write #\Newline :escape nil)))))))
  
