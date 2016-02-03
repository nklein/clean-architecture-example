;;; Console front-end

(defun console-prompt ()
  (fresh-line)
  (princ "BOOK-DB> ")
  (finish-output))

(defun console-quit (&optional (returning t))
  (throw 'console-quit returning))

(defun console-read ()
  (console-prompt)
  (handler-case
      (read)
    (end-of-file ()
      (console-quit nil))))

(defun console-print-summary (summary)
  (check-type summary book-summary)
  (format t "~A (~A) ~A~%"
          (book-summary-title summary)
          (book-summary-isbn summary)
          (book-summary-author summary)))

(defun console-browse-books ()
  (let ((request (make-browse-books-request))
        (response (make-browse-books-response)))
    (browse-books *browse-books-use-case* request response)
    (mapcar #'console-print-summary
            (browse-books-response-list-of-book-summaries response))
    (values)))

(defun console-eval (form)
  (flet ((browsep (form)
           (eql form :browse))
         (quitp (form)
           (member form '(#+ccl ccl:quit #+sbcl sbcl:exit :quit))))
    (cond
      ((browsep form)
       (console-browse-books))
      ((and (listp form) (quitp (first form)))
       (apply #'console-quit (rest form)))
      ((and (atom form) (quitp form))
       (console-quit))
      (t
       (eval form)))))

(defun console-print (value)
  (pprint value))

(defun console-main-loop ()
  (catch 'console-quit
    (with-standard-io-syntax
      (loop
         :do (mapc #'console-print
                   (multiple-value-list (console-eval (console-read))))))))
