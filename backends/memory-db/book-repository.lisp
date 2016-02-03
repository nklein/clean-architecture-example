;;; In-memory book repository implementation

(defclass in-memory-book-repository (book-repository)
  ((books :initarg :books :reader books)))

(defun make-in-memory-book-repository (books)
  (check-type books list)
  (assert (every #'book-p books))
  (make-instance 'in-memory-book-repository :books books))

(defun maybe-copy-book (book)
  (when book
    (validate book)
    (copy-book book)))

(defmethod find-book-by-isbn ((book-repository in-memory-book-repository)
                              (isbn string))
  (maybe-copy-book (find isbn (books book-repository)
                         :key #'book-isbn)))

(defmethod all-books ((book-repository in-memory-book-repository))
  (mapcar #'maybe-copy-book (books book-repository)))
