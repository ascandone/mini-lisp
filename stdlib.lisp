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

(defun foldl (lst z f)
  (if (atom? lst)
    z
    (foldl
      (tail lst)
      (f z (head lst))
      f)))

(defun foldr (lst z f)
  (if (atom? lst)
    nil
    (f
      (head lst)
      (foldr (tail lst) z f))))

(defun sum (lst)
  (foldl lst 0 +))

(defun reverse (lst)
  (foldl lst nil
    (lambda (acc x) (cons x acc))))

(defun map (lst f)
  (foldr lst nil
    (lambda (hd tl)
      (cons (f hd) tl))))

(defun inc (x) (+ 1 x))
