#lang racket
(require "mykanren.rkt")
(require rackunit)




(check-equal?(run 1 (q) (== 'olive q) (== 'oil q)) '())

(check-equal? (run 1 (q) (== 'olive q))
              '((olive)))

(check-equal? (run 4237 (q) (== 'olive q))
              '((olive)))

(check-equal? (run 0 (q) (== 'olive q))
              '())

(defrel (conso f r out)
  (== (cons f r) out))

(check-equal? (run* q (conso 1 q '(1 2 3))) '((2 3)))

(defrel (caro l out)
  (fresh (r)
         (conso out r l)))

(defrel (cdro l out)
  (fresh (f)
         (conso f out l)))

(defrel (nullo l)
  (conde
   [(== l '())]))

(defrel (appendo l t out)
  (conde
   [(nullo l)(== t out)]
   [(fresh (a d res)
           (conso a d l)
           (conso a res out)
           (appendo d t res))]))

(check-equal? (run 5 (q w) (appendo q w '(1 2 3)))
              '((() (1 2 3))
                ((1) (2 3))
                ((1 2) (3))
                ((1 2 3) ())))

(check-equal? (run 5 (q w e) (appendo q w e))
              '((() _0 _0)
                ((_0) _1 (_0 . _1))
                ((_0 _1) _2 (_0 _1 . _2))
                ((_0 _1 _2) _3 (_0 _1 _2 . _3))
                ((_0 _1 _2 _3) _4 (_0 _1 _2 _3 . _4))))