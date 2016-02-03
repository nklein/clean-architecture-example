;;; Simple interface to validate things

(define-condition invalid-thing-error (type-error)
  ((thing :initarg :thing :reader invalid-thing-error-thing)
   (problem :initarg :problem :reader invalid-thing-error-problem)))

(defgeneric validate (something)
  (:method (something) something)
  (:method ((something null))
    (error 'invalid-thing-error :thing something :problem "Null is invalid.")))
