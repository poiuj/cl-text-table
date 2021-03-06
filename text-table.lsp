;;;; text-table package implementation
;;;; Copyright (C) 2013  Vadym Khoptynets <vadya.poiuj@gmail.com>

;;;; This file is part of cl-text-table

;;;; cl-text-table is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License along
;;;; with this program; if not, write to the Free Software Foundation, Inc.,
;;;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

(in-package text-table)

;; table -> (title row row ... row)
;; title, row -> #(element element ... element)

(defun add-row (row tbl)
  "Adds row to the table"
  (declare (array row)
           (list tbl))
  
  (let ((row-list (list row)))
    (if tbl
        (if (= (length (car tbl)) (length row))
            (append tbl row-list)
            (error "Row lengths doesn't match!"))
        row-list)))


(defun print-table (tbl &key (header-delimiter #\=) (column-delimiter #\|) (row-delimiter #\Newline) (extra-ws 2) (stream *standard-output*))
  "Prints table"
  (macrolet ((write-no-escape (object)
               `(write ,object :stream stream :escape nil)))
    
    (flet ((%print-row (row lengths)
             (loop 
                for element across row
                for element-length across lengths do
                  (let* ((avail-ws (- element-length (length element)))
                         (ws-before (round (/ avail-ws extra-ws)))
                         (ws-after (- avail-ws ws-before)))
                    (loop repeat ws-before do (write-no-escape #\Space))
                    (write-no-escape element)
                    (loop repeat ws-after do (write-no-escape #\Space))
                    (write-no-escape column-delimiter))))

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
              (write-no-escape row-delimiter)
              
              ;; print horizontal delimiter
              (let ((total-length (+ (loop for x across lengths sum x)
                                     header-length)))
                (loop repeat total-length do (write-no-escape header-delimiter)))
              (write-no-escape row-delimiter)
              
              ;; print rest table
              (loop for row in (cdr tbl) do
                   (%print-row row lengths)
                   (write-no-escape row-delimiter))))))))


(defun parse (&key (stream *standard-input*) (row-delimiter #\Newline) (column-delimiter #\;))
  "Reads table"
  (flet ((%list-to-array (list)
           (make-array (length list)
                       :initial-contents list)))
    
    (macrolet ((read-next ()
                 `(peek-char nil stream nil)))
      
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
                         (read-char stream nil))
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
                               (read-char stream nil))
                       (return (coerce (nreverse element) 'string))
                     do
                       (push (read-char stream nil) element))
                  row))
            table)))))


(defun parse-file (file-name &rest rest)
  "Shorthand to parse table from specified file"
  (with-open-file (stream file-name)
    (apply #'parse :stream stream rest)))
