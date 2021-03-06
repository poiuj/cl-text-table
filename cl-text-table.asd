;;;; cl-text-table system definition
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

(defsystem cl-text-table
  :name "cl-text-table"
  :description "Library for printing text tables"
  :author "Vadym Khoptynets <vadya.poiuj@gmail.com>"
  :version "1.0.0"
  :license "GPLv2"
  :components ((:file "package" :type "lsp")
               (:file "text-table" :type "lsp"
                      :depends-on 
                      ("package"))))
