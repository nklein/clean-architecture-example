;;; Repository interface

(defclass book-repository ()
  ())

(defgeneric find-book-by-isbn (book-repository isbn))
(defgeneric all-books (book-repository))
