;;; Simple data structures (no intelligence except maybe validation)

(defstruct book
  isbn
  title
  author
  cover-page-thumbnail
  cover-page-fullsize
  list-of-thumbnails
  table-of-contents
  synopsis
  publication-date
  list-of-genres
  list-of-keywords)
