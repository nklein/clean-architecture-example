;;; Use case interface

(defstruct browse-books-request)

(defstruct browse-books-response
  list-of-book-summaries)

(defclass browse-books-use-case ()
  ())

(defgeneric browse-books (use-case request response))
