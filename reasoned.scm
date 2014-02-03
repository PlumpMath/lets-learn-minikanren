;; Working through the reasoned schemer
;; using Petite Chez Scheme Version 8.4
;; took mk.scm from https://github.com/webyrd/quines
;; which is why I'm using petite because he did and it works.
(load "mk.scm")

(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons a d) p))))

(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

