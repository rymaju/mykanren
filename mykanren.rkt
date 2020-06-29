#lang racket
(require rackunit)
;; Ryan Jung : June 28th, 2020
;; my version of minikanren, following The Reasoned Schemer instructions verbatim
(provide run run*
         ==
         fresh
         conde conda condu
         disj conj disj2 conj2
         defrel
         succeed fail)

;; a Variable is a (vector Symbol)
;; that represents a logic variable
(define (var? x) (vector? x))

;; var: Symbol -> Variable
;; produces a new variable with the given symbol
(define (var x) (vector x))

(define u (var 'u))
(define v (var 'v))
(define w (var 'w))
(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

;; a Literal is one of:
;; - Symbol
;; - String
;; - Number
;; - Boolean
;; - [List-of Literal]

;; a Value is one of:
;; - Literal
;; - Variable
;; - [List-of Value]


;; an Association is a (cons Variable Value)
;; representing a pair of a Variable to its equivalent
;; Value

;; a Substitution is a [List-of Association]
;; representing a list of associations of Variables
;; to their fused value(s).
;; INVARIANT: cannot contain two or more assocaitions
;; with the same car. Also, it cannot contain any cyclic associations.

(define empty-s '()) ; the Substitution that contains no associations
(define sub1 `((,x . ,y) (,v . ,x) (,w . ,x)))
(define sub2 `((,x . b) (,z . ,y) (,w . (,x e ,z))))


;; walk : Variable Substitution -> Value
;; produces the first Value fused to v in s
;; If a (walk x s) -> x, we know x is fresh
(define (walk v s)
  (local [(define a (and (var? v) (assv v s)))]
    (cond
      [(pair? a)(walk (cdr a) s)]
      [else v])))
(check-equal? (walk x sub1) y)
(check-equal? (walk w sub2) `(,x e ,z))

;; occurs? Variable Value Substitution -> Boolean
;; does x occur in v given s?
(define (occurs? x v s)
  (let [(v (walk v s))]
    (cond
      [(var? v) (eqv? v x)]
      [(pair? v)
       (or (occurs? x (car v) s)
           (occurs? x (cdr v) s))]
      [else #f])))

(check-equal? (occurs? x x '()) #t)
(check-equal? (occurs? x `(,y) `((,y . ,x))) #t)


;; ext-s : Variable Value Substitution -> Substituion | False
;; extends a substitution s with an association between x and v
;; produces #f if the extension creates a cycle
(define (ext-s x v s)
  (cond
    [(occurs? x v s) #f]
    [else (cons `(,x . ,v) s)]))
(check-equal? (ext-s x `(,x) empty-s) #f)
(check-equal? (ext-s x 1 empty-s) `((,x . 1)))
(check-equal? (ext-s x 1 sub1)
              `((,x . 1)(,x . ,y) (,v . ,x) (,w . ,x)))

;; unify: Value Value Substitution -> Substitution 
;; unifies the two values in the existing substitution s,
;; or returns false if the unification is invalid
(define (unify u v s)
  (let [(u (walk u s)) (v (walk v s))]
    (cond
      [(eqv? u v) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (let [(s (unify (car u) (car v) s))]
         (and s ; what is this and..?
              (unify (cdr u) (cdr v) s)))]
      [else #f])))

(check-equal? (unify x 1 empty-s) `((,x . 1)))
(check-equal? (unify 1 x empty-s) `((,x . 1)))
(check-equal? (unify x x empty-s) '())
(check-equal? (unify 1 2 empty-s) #f)
(check-equal? (unify x y `((,x . 1)(,y . 2))) #f)


;; BREAK 1

;; A [Stream-of X] is one of:
;; - '()
;; - (cons X [Stream-of X])
;; - [Suspension-of X]
;; and represents a possibly non-complete list of elements
;; that may need to be unwrapped (called) for more elements

;; A [Suspension-of X] is a [() -> [Stream-of X]]
;; represents a suspended stream of values
;; when the suspension is called to reveal the value inside,
;; we say that it is "forced"
(define suspension1 (lambda () `()))
(define suspension2 (lambda () `(d e)))
(define suspension3 (lambda () `(a b c ,suspension2)))

;; A Goal is a [Substitution -> [Stream-of Substitution]
;; and represents list of solutions to a relation 

;; == : Value Value -> Goal
;; produces a singleton Stream of the substitution that unifies u and v,
;; or the empty list if unification is impossible
(define (== u v)
  (lambda (s)
    (let [(s (unify u v s))]
      (if s `(,s) '()))))


(define succeed
  (lambda (s)
    `(,s)))

(define fail
  (lambda (s)
    `()))

(check-equal? ((== #t #f) empty-s) '())
(check-equal? ((== #f #f) empty-s) '(()))
(check-equal? ((== x y) empty-s) `(((,x . ,y))))

;; BREAK 2

;; append-inf : [Stream-of Substitution] [Stream-of Substitution] -> [Stream-of Substitution]
;; combines both Streams into a single stream
(define (append-inf s-inf t-inf)
  (cond
    [(null? s-inf) t-inf]
    [(pair? s-inf)
     (cons (car s-inf)
           (append-inf (cdr s-inf) t-inf))]
    [else (lambda ()
            (append-inf t-inf (s-inf)))]))
;; note to self explaining line 3:
;; if you hit a lambda, you need to replace it with a lambda,
;; so you cant actually take any more elements from s-inf,
;; instead in the lambda, grab as much as you can from t-inf, then
;; force the suspension.

;; disj2 : Goal Goal -> Goal
;; produces the combined goal of acheiving g1 or g2
(define (disj2 g1 g2)
  (lambda (s)
    (append-inf (g1 s) (g2 s))))

(check-equal? ((disj2 (== 'olive x) (== 'oil x)) empty-s)
              `(((,x . olive))((,x . oil))))

;; nevero : [() -> Goal]
;; represents a relation, which returns a stream of suspensions
;; that never resolves to any value
(define (nevero)
  (lambda (s)
    (lambda ()
      ((nevero) s))))

;; alwayso : [() -> Goal]
;; represents a relation, which returns an infinite
;; stream of suspended succeed's
(define (alwayso)
  (lambda (s)
    (lambda ()
      ((disj2 succeed (alwayso)) s))))

;; take-inf : (Natural | False) [Stream-of X] -> [List-of X]
;; produces at most the first n elements from the stream.
;; if n is #f, then take-inf produces all elements in the stream
;; MAY RECUR INFINITELY if less than n values exist
(define (take-inf n s-inf)
  (cond
    [(and n (zero? n)) '()]
    [(null? s-inf) '()]
    [(pair? s-inf)
     (cons (car s-inf)
           (take-inf (and n (- n 1))
                      (cdr s-inf)))]
    [else (take-inf n (s-inf))]))
(check-equal? (take-inf 3 ((alwayso) empty-s)) '(()()()))
(check-equal? (take-inf #f `(a b c d e)) `(a b c d e))
(check-equal? (take-inf 0 ((alwayso) empty-s)) '())

;; BREAK 3

;; append-map-inf : Goal [Stream-of Substitution] -> [Stream-of Substitution]
;; produces a stream of the appended result of applying every substitution in s-inf to g
(define (append-map-inf g s-inf)
  (cond
    [(null? s-inf) '()]
    [(pair? s-inf)
     (append-inf (g (car s-inf))
                 (append-map-inf g (cdr s-inf)))]
    [else (lambda ()
            (append-map-inf g (s-inf)))]))

;; conj2 : Goal Goal -> Goal
;; produces the combined goal of acheiving g1 and g2
(define (conj2 g1 g2)
  (lambda (s)
    (append-map-inf g2 (g1 s))))
(check-equal? ((conj2 (== x y) (== y 1)) empty-s) `(((,y . 1) (,x . ,y))))
(check-equal? ((conj2 (== x y) (conj2 (== y 1) (== x 2))) empty-s) '())

;; BREAK 4

;; call/fresh : Symbol [Variable -> Goal]
;; introduces a variable with the given name to the function
(define (call/fresh name f)
  (f (var name)))

(check-equal? (take-inf 1 ((call/fresh 'kiwi (lambda (fruit) (== fruit 'plum))) empty-s))
              `(((,(var 'kiwi) . plum))))

;; reify-name : NonNegativeInteger -> Symbol
;; produces the symbol that represents the reified variable numbered n
(define (reify-name n)
  (string->symbol
   (string-append "_"
                  (number->string n))))

(check-equal? (reify-name 0) '_0)
(check-equal? (reify-name 1) '_1)
(check-equal? (reify-name 2) '_2)
(check-equal? (reify-name 42) '_42)

;; walk* : Variable Substitution -> Value
;; recursively travels through s to find the associated value of v
(define (walk* v s)
  (let [(v (walk v s))]
    (cond
      [(var? v) v]
      [(pair? v)
       (cons
        (walk* (car v) s)
        (walk* (cdr v) s))]
      [else v])))


;; reify-s : Variable Substitution -> Substitution
;; produces a reified-name substituion based on v and the association in r
(define (reify-s v r)
  (let [(v (walk v r))]
    (cond
      [(var? v)
       (let [(n (length r))]
         (let [(rn (reify-name n))]
           (cons `(,v . ,rn) r)))]
      [(pair? v)
       (let [(r (reify-s (car v) r))]
             (reify-s (cdr v) r))]
      [else r])))

;; reify : Variable -> [Substitution -> Literal]]
;; produces a successful subsitution of v replacing each fresh variable with a reified name
(define (reify v)
  (lambda (s)
    (let [(v (walk* v s))]
      (let [(r (reify-s v empty-s))]
        (walk* v r)))))
(check-equal? ((reify x) `((,x . (1 2 3 4 5 ,y)))) '(1 2 3 4 5 _0))
(check-equal? ((reify x) empty-s) '_0)



;; run-goal : Natural Goal -> [List-of Substitution]
;; takes the first n substitutions that succeed in acheiving g
(define (run-goal n g)
  (take-inf n (g empty-s)))
(check-equal? (run-goal 1 (disj2 (== 'olive y) (== 'oil x))) `(((,y . olive))))
(check-equal? (run-goal 5 (disj2 (== 'olive y) (== 'oil x))) `(((,y . olive)) ((,x . oil))))

;; BREAK 5

;; ifte (if, then, else) : Goal Goal Goal -> Goal
;; if the first goal succeeds, proceeds to the second, or else the third
(define (ifte g1 g2 g3)
  (lambda (s)
     (let loop ([s-inf (g1 s)])
       (cond
         [(null? s-inf) (g3 s)]
         [(pair? s-inf)
          (append-map-inf g2 s-inf)]
         [else (lambda ()
                      (loop (s-inf)))]))))

(check-equal? ((ifte succeed
                     (== #f y)
                     (== #t y))
               empty-s) `(((,y . #f))))
(check-equal? ((ifte fail
                     (== #f y)
                     (== #t y))
               empty-s) `(((,y . #t))))

;; once : Goal -> Goal
;; returns a goal containing only the first substitution of g
(define (once g)
  (lambda (s)
         (let loop ([s-inf (g s)])
           (cond
             [(null? s-inf) '()]
             [(pair? s-inf)
              (cons (car s-inf) '())]
             [else (lambda ()
                     (loop (s-inf)))]))))
(check-equal? ((once (disj2 (== #t x) (== #f x))) empty-s)
              `(((,x . #t))))
           

; THE END

;; Defining the language syntax:

(define-syntax disj
  (syntax-rules ()
    ((disj) fail)
    ((disj g) g)
    ((disj g0 g ...) (disj2 g0 (disj g ...)))))

(define-syntax conj
  (syntax-rules ()
    ((conj) succeed)
    ((conj g) g)
    ((conj g0 g ...) (conj2 g0 (conj g ...)))))

(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (lambda (s)
         (lambda ()
           ((conj g ...) s)))))))

(define-syntax run
  (syntax-rules ()
    ((run n (x0 x ...) g ...)
     (run n q (fresh (x0 x ...)
                     (== `(,x0 ,x ...) q) g ...)))
    ((run n q g ...)
     (let ((q (var 'q)))
       (map (reify q)
            (run-goal n (conj g ...)))))))

(define-syntax run*
  (syntax-rules ()
    ((run* q g ...) (run #f q g ...))))

(define-syntax fresh
  (syntax-rules ()
    ((fresh () g ...)(conj g ...))
    ((fresh (x0 x ...) g ...)
     (call/fresh 'x0 ;;???
                 (lambda (x0)
                   (fresh (x ...) g ...))))))

(define-syntax conde
  (syntax-rules ()
    ((conde (g ...) ...)
     (disj (conj g ...) ...))))

(define-syntax conda
  (syntax-rules ()
    ((conda (g0 g ...)) (conj g0 g ...))
    ((conda (g0 g ...) ln ...)
     (ifte g0 (conj g ...) (conda ln ...)))))

(define-syntax condu
  (syntax-rules ()
    ((condu (g0 g ...) ...)
     (conda ((once g0) g ...) ...))))

;; Roll credits!
