## Ocaml lisp interpreter

**WIP**

### Basic syntax

```lisp
;; Atoms:
42 ; => 42
#'c' ; => #'c'
x ; => ## Error: unbound value: x
() ; => nil
`x ; => x
`(+ 1 2) ; => (+ 1 2)
`(1 2 ,x) ; => ## Error: unbound value: x
"hello" ; => "hello"
; strings are actually sugar for lists of chars, e.g. `(#'h', #'e', #'l', #'l', #'o')

; bools are encoded with:
true ; => 'true
false ; => nil

;; S-expressions:

(def x 42) ; => nil (defines a global value x := 42)

; function application
(+ 1 2) ; => 3

; function declaration
(lambda (x) (+ 1 x)) ; => [[Lambda]]

; module require
(require "path/to/file.lisp")
```

### Functions syntax sugar
```lisp
; optional argument
(defun f (a &option b) (list a b))
(f 0) ; => (0 nil)
(f 0 1) ; => (0 1)

; variadic arguments
(defun f (a &rest as) (list a as))
(f 0) ; => (0 nil)
(f 0 1) ; => (0 (1))
(f 0 1 2 3) ; => (0 (1 2 3))

; labeled arguments
(defun f (a &key x y) (list a x y))
(f 0 'x 1 'y 2) ; => (0 1 2)
(f 0 'y 2) ; => (0 nil 2)
(f 0) ; => (0 nil nil)

; Destructuring
(defun f ((x y z)) x)
(f '(1 2 3)) ; => 1

(defun f ((x)) x)
(f '(1 2 3)) ; => ## Error: Wrong number of args (1) passed to lambda

(defun f ((hd &rest tl)) (list hd tl))
(f '(1 2 3)) ; => (1 (2 3))
```

### Macros
```lisp
; (unless true `a `b) expands to:
; (if true `b `a)

(defmacro unless (b x y)
  `(if ,b ,y ,x))
```

