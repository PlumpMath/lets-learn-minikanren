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

(define nullo
  (lambda (x)
    (== `() x)))

(define eqo
  (lambda (x y)
    (== x y)))

(define pairo
  (lambda (p)
    (fresh (a d)
       (conso a d p))))

;; In terms of conso
(define caro
  (lambda (p a)
    (fresh (x)
       (conso a x p))))

(define cdro
  (lambda (p a)
    (fresh (x)
       (conso x a p))))

;; listo
(define listo
  (lambda (l)
    (conde
     ((nullo l) succeed)
     ((pairo l)
      (fresh (d)
         (cdro l d)
         (listo d)))
     (succeed fail))))

(define lol?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((list? (car l)) (lol? (cdr l)))
     (else #f))))

(define lolo
  (lambda (l)
    (conde
     ((nullo l) succeed)
     ((fresh (x y)
         (caro l x)
         (cdro l y)
         (listo x)
         (lolo y)))
     (succeed fail))))

(define twinso
  (lambda (l)
    (fresh (x y)
       (conso x y l)
       (conso x `() y))))

(define twinso
  (lambda (l)
    (fresh (x)
           (== `(,x ,x) l))))

(define eq-caro
  (lambda (l x)
    (caro l x)))

(define membero
  (lambda (x l)
    (conde
     ((eq-caro l x) succeed)
     ((nullo l) fail)
     (succeed
      (fresh (d)
             (cdro l d)
             (membero x d))))))

(define identity
  (lambda (l)
    (run* (y) (membero y l))))

(define eq-car?
  (lambda (l x)
    (eq? (car l) x)))

(define rember
  (lambda (x l)
    (cond
     ((null? l) `())
     ((eq-car? l x) (cdr l))
     (else
      (cons (car l)
        (rember x (cdr l)))))))

(define rembero
  (lambda (x l out)
    (conde
     ((nullo l) (== `() out))
     ((eq-caro l x) (cdro l out))
     (succeed
       (fresh (a d res)
         (cdro l d)
         (rembero x d res)
         (caro l a)
         (conso a res out))))))

(define appendo
  (lambda (l s out)
    (conde
     ((nullo l) (== s out))
     (succeed
       (fresh (f r c)
         (conso f r l)
         (conso f c out)
         (appendo r s c))))))

(define unwrapo
  (lambda (x out)
    (conde
     ((pairo x)
      (fresh (r)
        (caro x r)
        (unwrapo r out)))
     (succeed (== x out)))))

(define flatteno
  (lambda (s out)
    (conde
     ((nullo s) (==`() out))
     ((pairo s)
      (fresh (a d res-a res-d)
        (conso a d s)
        (flatteno a res-a)
        (flatteno d res-d)
        (appendo res-a res-d out)))
     (succeed (conso s `() out)))))

