;;; Application initialization

(defvar *book-list*
  (list (make-book :isbn "1111122223"
                   :title "Book One"
                   :author "Primo Librazzi"
                   :cover-page-thumbnail "thumb-book-one"
                   :cover-page-fullsize "full-book-one"
                   :list-of-thumbnails '()
                   :table-of-contents "T.O.C."
                   :synopsis "Book one is the first book in the Book
                              One trilogy."
                   :publication-date 2007
                   :list-of-genres '(:mystery)
                   :list-of-keywords '(:one :book :acclaimed))
        (make-book :isbn "0123456789"
                   :title "Book Two"
                   :author "Segundo Librazzi"
                   :cover-page-thumbnail "thumb-book-two"
                   :cover-page-fullsize "full-book-two"
                   :list-of-thumbnails '()
                   :table-of-contents "T.O.C.#2"
                   :synopsis "Book two is the second book in the Book
                              One trilogy."
                   :publication-date 2015
                   :list-of-genres '(:mystery :biography)
                   :list-of-keywords '(:one :two :book :long-awaited))))

(defun run-console-app-with-memory-db (&optional (books *book-list*))
  (let* ((book-repo (make-in-memory-book-repository books))
         (*browse-books-use-case* (make-browse-books-use-case book-repo)))
    (console-main-loop)))
