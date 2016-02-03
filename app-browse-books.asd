;;; Application interfaces

(asdf:defsystem app-data-objects
  :description "Sample application using Clean Architecture (Data Objects)."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20160202"
  :license "UNLICENSE"
  :depends-on ()
  :components ((:file "data-objects/validate")
               (:file "data-objects/book-summary")
               (:file "data-objects/book")))

(asdf:defsystem app-backend-interfaces
  :description "Sample application using Clean Architecture (Backend I/F)."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20160202"
  :license "UNLICENSE"
  :depends-on (#:app-data-objects)
  :components ((:file "use-cases/browse-books/book-repository")))

(asdf:defsystem app-use-case-interfaces
  :description "Sample application using Clean Architecture (Use-Case I/F)."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20160202"
  :license "UNLICENSE"
  :depends-on (#:app-data-objects)
  :components ((:file "app-context")
               (:file "use-cases/browse-books/browse-books")))

;;; Application implementation

(asdf:defsystem app-logic
  :description "Sample application using Clean Architecture (App Logic)."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20160202"
  :license "UNLICENSE"
  :depends-on (#:app-data-objects
               #:app-backend-interfaces
               #:app-use-case-interfaces)
  :components ((:file "use-cases/browse-books/browse-books-impl")))

;;; Back-end implementations

(asdf:defsystem app-memory-db-backend
  :description "Sample application using Clean Architecture (Memory Db)."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20160202"
  :license "UNLICENSE"
  :depends-on (#:app-data-objects #:app-backend-interfaces)
  :components ((:file "backends/memory-db/book-repository")))

;;; Front-end implementations

(asdf:defsystem app-console-frontend
  :description "Sample application using Clean Architecture (Console Front)."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20160202"
  :license "UNLICENSE"
  :depends-on (#:app-data-objects #:app-use-case-interfaces)
  :components ((:file "frontends/console/console-frontend")))

;;; Top-level application

(asdf:defsystem app-browse-books
  :description "Sample application using Clean Architecture (Top-Level)."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20160202"
  :license "UNLICENSE"
  :depends-on (#:app-logic
               #:app-memory-db-backend
               #:app-console-frontend)
  :components ((:static-file "README.md")
               (:file "app")))
