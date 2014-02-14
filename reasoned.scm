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

(define flattenrev
  (lambda (s out)
    (conde
     (succeed (conso s `() out))
     ((nullo s) (==`() out))
     ((pairo s)
      (fresh (a d res-a res-d)
        (conso a d s)
        (flatteno a res-a)
        (flatteno d res-d)
        (appendo res-a res-d out))))))

;; Real confused right now

;; chapter 6
(define anyo
  (lambda (g)
    (conde
     (g succeed)
     (succeed (anyo g)))))

(define nevero (anyo fail))

(define alwayso (anyo succeed))

(define salo
  (lambda (g)
    (conde
     (succeed succeed)
     (succeed g))))

;; chapter 7
(define bit-xoro
  (lambda (x y r)
    (conde
     ((== 0 x) (== 0 y) (== 0 r))
     ((== 1 x) (== 0 y) (== 1 r))
     ((== 0 x) (== 1 y) (== 1 r))
     ((== 1 x) (== 1 y) (== 0 r))
     (succeed fail))))

;; (run* (s) (fresh (x y) (bit-xoro x y 0) (== s `(,x ,y))))
;; (run* (s) (fresh (x y) (bit-xoro x y 1) (== s `(,x ,y))))

(define bit-ando
  (lambda (x y r)
    (conde
     ((== 0 x) (== 0 y) (== 0 r))
     ((== 1 x) (== 0 y) (== 0 r))
     ((== 0 x) (== 1 y) (== 0 r))
     ((== 1 x) (== 1 y) (== 1 r))
     (succeed fail))))


;; don't know what and is, think prolly the second of these is the
;; correct way to doit.

;; (define half-addero
;;   (lambda (x y r c)
;;     (conde ((bit-xoro x y r)
;;             (bit-ando x y c))
;;            (succeed fail))))

(define half-addero
  (lambda (x y r c)
    (fresh ()
      (bit-xoro x y r)
      (bit-ando x y c))))

(define full-addero
  (lambda (b x y r c)
    (fresh (w xy wz)
      (half-addero x y w xy)
      (half-addero w b r wz)
      (bit-xoro xy wz c))))

(define build-num
  (lambda (n)
    (cond
     ((zero? n) '())
     ((odd? n) (cons 1 (build-num (/ (- n 1) 2))))
     ((even? n) (cons 0 (build-num (/ n 2)))))))

(define poso
  (lambda (n)
    (fresh (a d)
      (== (cons a d) n))))

(define >1o
  (lambda (x)
    (fresh (a ad dd)
      (== `(,a ,ad . ,dd) x))))

;; (define addero
;;   (lambda (d n m r)
;;     (conde
;;      ((== 0 d) (== `() m) (== n r))
;;      ((== 0 d) (== `() n) (== m r) (poso m))
     
;;      )
;;     ))
