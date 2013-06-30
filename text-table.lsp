(in-package text-table)

;; table -> (title row row ... row)
;; title, row -> #(element element ... element)

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

(defconstant extra-ws 2)

(defun print-table (tbl &optional (delimiter #\|))
  "prints table 'tbl' with delimiter 'delimiter' to the standard output"
  (macrolet ((write-no-escape (object)
               `(write ,object :escape nil)))
    
    (flet ((%print-row (row lengths)
             (loop for element across row
                for element-length across lengths do
                  (let* ((avail-ws (- element-length (length element)))
                         (ws-before (round (/ avail-ws extra-ws)))
                         (ws-after (- avail-ws ws-before)))
                    (loop repeat ws-before do (write-no-escape #\Space))
                    (write-no-escape element)
                    (loop repeat ws-after do (write-no-escape #\Space))
                    (write-no-escape delimiter))))

           (%find-widest-item (tbl element-number)
             (loop for x in tbl maximize (length (aref x element-number)))))

      (let* ((header (car tbl))
             (header-length (length header)))
        (if tbl
            (let ((lengths (make-array header-length :element-type 'integer)))
              (dotimes (i (length (car tbl)))
                ;; add whitespaces before and after: +extra-ws
                (setf (aref lengths i)
                      (+ (%find-widest-item tbl i) extra-ws)))
              
              ;; print header
              (%print-row header lengths)
              (write-no-escape #\Newline)
              
              ;; print horizontal delimiter
              (let ((total-length (+ (loop for x across lengths sum x)
                                     header-length)))
                (loop repeat total-length do (write-no-escape #\=)))
              (write-no-escape #\Newline)
              
              ;; print rest table
              (loop for row in (cdr tbl) do
                   (%print-row row lengths)
                   (write-no-escape #\Newline))))))))


(defun parse-file (file-name &key (row-delimiter #\Newline) (column-delimiter #\;))
  "creates table from text file"
  (flet ((%list-to-array (list)
           (make-array (length list)
                       :initial-contents list)))

    (macrolet ((read-next ()
                 `(peek-char nil s nil)))

      (with-open-file (s file-name)
        ;;read whole table
        (loop 
           with table
           while (read-next)
           finally (return (nreverse table))
           do
             (push
              ;;read row
              (loop 
                 with row
                 until (let ((next-char (read-next)))
                         (or (eq next-char row-delimiter)
                             (not next-char)))
                 finally (when (eq (read-next) row-delimiter)
                           (read-char s nil))
                   (return (%list-to-array (nreverse row)))
                 do
                   (push
                    ;;read element
                    (loop 
                       with element
                       until (let ((next-char (read-next)))
                               (or (eq next-char column-delimiter)
                                   (eq next-char row-delimiter)
                                   (not next-char)))
                       finally (when (eq (read-next) column-delimiter)
                                 (read-char s nil))
                         (return (coerce (nreverse element) 'string))
                       do
                         (push (read-char s nil) element))
                    row))
              table))))))
