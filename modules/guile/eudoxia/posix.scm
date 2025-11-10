(define-module (eudoxia posix)
  #:export (path 1l))

(define (path)
  (string-split (getenv "PATH") #\:))

(define (1l lst)
  (for-each (lambda (e) (display e) (newline)) lst))
