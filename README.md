# mykanren

## Racket implementation of minikanren using The Reasoned Schemer Second Edition syntax.

This is the minikanren language as used in The Reasoned Schemer Second Edition. Unlike other implementations in Racket, this implementation includes `defrel`, `conj2`, `disj2`, `conj`, and `disj` to make following the examples in the book slightly easier.

To use, import `mykanren.rkt` with `(require "mykanren.rkt")` (see `example.rkt`).

Also, feel free to take a look inside `mykanren.rkt`, which has been annotated in `HtDP` style. Note that I have no idea what I'm doing so if something is painfully wrong then edits are more than welcome.

## Provided

- [`run`](#run)
- [`run*`](#run-star)
- [`==`](#equiv)
- [`defrel`](#defrel)
- [`disj2`](#disj2)
- [`conj2`](#conj2)
- [`disj`](#disj)
- [`conj`](#conj)
- [`fresh`](#fresh)
- [`conde`](#conde)
- [`conda`](#conda)
- [`condu`](#condu)
- [`success`](#success)
- [`fail`](#fail)

### `run`

`(run n q g ...)` or `(run n (q ...) g ...)`

Returns at most `n` possible solutions that successfully satisfy goal g _and_ goals in `...`. If less than `n` solutions exist, then recurs infinitely (no value).

```Racket
(run 1 q (== 'olive q)) -> '((olive))

(run 1 q (== 'olive q) (== 'oil q)) -> '()

(run 1 (q v) (== 'olive q) (== 'oil v)) -> '((olive oil))

(run 4237 q (== 'olive q)) -> '((olive))

(run 0 q (== 'olive q)) -> '()

(run 2 (q w) (appendo q w '(1 2 3))) -> '((() (1 2 3))
                                          ((1) (2 3)))

```

<div id="run-star"/>

### `run*`

`(run* q g ...)` or `(run* (q ...) g ...)`

`[List-of Name] Goal (...Goal) -> [List-of [List-of Literal]]`

Returns all possible solutions that successfully satisfy goal g _and_ goals in `...`. If no solutions exist, then recurs infinitely (no value).

Shorthand for `(run #f (q) g ...)`.

```Racket
(run* q (== 'olive q) (== 'oil q)) -> '()

(run* (q) (== 'olive q)) -> '((olive))

(run* (q w) (appendo q w '(1 2 3))) -> '((() (1 2 3))
                                        ((1) (2 3))
                                        ((1 2) (3))
                                        ((1 2 3)
                                        ()))
```

<div id="equiv"/>

### `==`

`(== u v)`

`Value Value -> Goal`

Associates value `u` with value `v`. Equivalent to `(== v u)`.

```Racket
(run* (q) (== 'olive q)) -> '((olive))

(run* (q) (== q 'olive)) -> '((olive))

(run* (x y z) (== `(,x ,y, z) '(1 2 3))) -> '((1 2 3))
```

### `defrel`

`(defrel (relation-name arg0 ...) ...)`

**Def**ines a **rel**ation between the arguments. The body of `defrel` must be a logical expression: it must evaluate to some `Goal`.

```Racket
(defrel (nullo l)
  (== l '()))

(defrel (conso f r out)
  (== (cons f r) out))

(defrel (caro l out)
  (fresh (r)
    (conso out r l)))

(defrel (cdro l out)
  (fresh (f)
    (conso f out l)))
```

### `disj2`

`(disj2 g1 g2)`

`Goal Goal -> Goal`

Produces the combined goal of acheiving `g1` _or_ `g2`.

```Racket
(run* (q) (disj2 (== 'olive q) (== 'oil q))) -> '((olive) (oil))
```

### `conj2`

`(disj2 g1 g2)`

`Goal Goal -> Goal`

Produces the combined goal of acheiving `g1` _and_ `g2`.

```Racket
(run* (q) (conj2 (== 'olive q) (== 'oil q))) -> '()

(run* (q w) (conj2 (== q '()) (appendo q w '(1 2 3)))) -> '((() (1 2 3)))
```

### `disj`

`(disj2 g ...)`

`(...Goal) -> Goal`

Like `disj2` but for an arbitrary number of arguments.

```Racket
(run* (q) (disj (== 'olive q))) -> '((olive))
(run* (q) (disj (== 'extra q)
                (== 'virgin q)
                (== 'olive q)
                (== 'oil q))) -> '((extra)
                                   (virgin)
                                   (olive)
                                   (oil))
```

### `conj`

`(disj2 g1 g2)`

`Goal Goal -> Goal`

Like `conj` but for an arbitrary number of arguments. Not paticularly useful, as this is the default behavior for `Goal`s in the body of `run`.

```Racket
(run* (q) (conj (== 'olive q))) -> '((olive))
```

### `fresh`

`(fresh (var-name ...) ...)`

Creates a **fresh** variable(s) `var-name`, which can be used within the body of `fresh`.

```Racket
(defrel (caro l out)
  (fresh (r)
    (conso out r l)))

(defrel (cdro l out)
  (fresh (f)
    (conso f out l)))
```

### `conde`

`(conde [g1 ...] [g2 ...] ...)`

Defines a series of goals, where each "branch" of the `conde` is in a `disj` (Branch A **OR** Branch B **OR** Branch C etc.), and each series of goal _within_ a branch is in a `conj` (Goal A **OR** Goal B **OR** Goal C etc.). Syntactically and behaviorlly similar to `cond`.

```Racket
(defrel (appendo l t out)
  (conde
    [(nullo l)(== t out)]
    [(fresh (a d res)
      (conso a d l)
      (conso a res out)
      (appendo d t res))]))
```

### `conda`

`(conda [g1 ...] [g2 ...] ...)`

Syntactically identical to `conde`, but only the first line that succeeds may contribute values. "_a_" stands for "_a_ single line", since at most only a single line can contribute values.

```Racket
(run* x
  (conda
    [(== 'olive x)]
    [(== 'oil x)])) -> '((olive))


(run* (q w x)
  (conda
    [(appendo q w '(1 2 3))]
    [(== 'oil x)]))          -> '((() (1 2 3) _0)
                                  ((1) (2 3) _0)
                                  ((1 2) (3) _0)
                                  ((1 2 3) () _0))
```

### `condu`

`(conde [g1 ...] [g2 ...] ...)`

Syntactically identical to `conde`, but a successful question succeeds only once. "_u_" stands for "_U_ will never *u*se this", as it only appears for 3 pages then vanishes.

Just kidding, it actually corresponds to Mercury's commited choice.

```Racket
(run* (q w x)
  (condu
    [(appendo q w '(1 2 3))]
    [(== 'oil x)]))          -> '((() (1 2 3) _0))

```

### `success`

Shorthand for `(== #t #t)`, represents a goal that is always successful.

```Racket
(run* q succeed) -> '(_0)
```

### `fail`

Shorthand for `(== #f #t)`, represents a goal that always fails.

```Racket
(run* q fail) -> '()
```
