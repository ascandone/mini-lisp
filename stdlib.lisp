(def true `true)
(def otherwise true)
(def nil `())
(def false nil)

(defmacro if (b x y)
  `(cond
      ,b ,x
      otherwise ,y))

(defmacro not (x)
  `(if ,x
    false
    true))

(defmacro and (x y)
  `(if ,x
    ,y
    false))

(defmacro or (x y)
  `(if ,x
    true
    ,y))


(defmacro defun (name params body)
  `(def ,name (lambda ,params ,body)))

(defun range (start end)
  (if (= start end)
      nil
      (cons start (range (+ 1 start) end))))

(defun map (f lst)
  (if (atom? lst)
    nil
    (cons
      (f (head lst))
      (map f (tail lst)))))

(defun sum (lst)
  (if (atom? lst)
    0
    (+
      (head lst)
      (sum (tail lst)))))
