;;; Use case implementation

(defun summarize-book (book)
  (check-type book book)
  (make-book-summary :isbn (book-isbn book)
                     :title (book-title book)
                     :author (book-author book)
                     :cover-page-thumbnail (book-cover-page-thumbnail book)))

(defclass browse-books-impl (browse-books-use-case)
  ((book-repository :initarg :book-repository :reader book-repository)))

(defun make-browse-books-use-case (book-repository)
  (check-type book-repository book-repository)
  (make-instance 'browse-books-impl :book-repository book-repository))

(defmethod browse-books ((use-case browse-books-impl)
                         (request browse-books-request)
                         (response browse-books-response))
  (let* ((books (all-books (book-repository use-case)))
         (summaries (mapcar #'summarize-book books)))
    (setf (browse-books-response-list-of-book-summaries response) summaries))
  response)
