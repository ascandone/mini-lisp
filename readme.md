## Ocaml lisp interpreter

**WIP**

### Basic syntax

```lisp
;; Atoms:
42 ; => 42
'c' ; => 'c'
x ; => ## Error: unbound value: x
() ; => nil
`x ; => x
`(+ 1 2) ; => (+ 1 2)
`(1 2 ,x) ; => ## Error: unbound value: x
"hello" ; => "hello"

;; S-expressions:

(def x 42) ; => nil (defines a global value x := 42)

; function application
(+ 1 2) ; => 3

; function declaration
(lambda (x) (+ 1 x)) ; => [[Lambda]]

; module require
(require "path/to/file.lisp")
```


### Macros
```lisp
; (unless true `a `b) expands to:
; (if true `b `a)

(defmacro unless (b x y)
  `(if ,b ,y ,x))
```