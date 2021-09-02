(def true 'true)
(def otherwise true)
(def nil '())
(def false nil)

(def list (lambda (& xs) xs))

(def foldr
  (lambda (lst z f)
    (cond
      (atom? lst) z
      otherwise (f
                  (head lst)
                  (foldr (tail lst) z f)))))

(def concat
  (lambda (& nested)
    (foldr
      nested
      nil
      (lambda (xs ys) (foldr xs ys cons)))))

(defmacro if (b x y)
  `(cond
      ~b ~x
      otherwise ~y))

(defmacro not (x)
  `(if ~x
    false
    true))

(defmacro and (x y)
  `(if ~x
    ~y
    false))

(defmacro or (x y)
  `(if ~x
    true
    ~y))

(defmacro defun (name params body)
  `(def ~name (lambda ~params ~body)))

(defun second (lst)
  (head (tail lst)))

(defmacro let- ((param value) body)
  `((lambda (~param) ~body) ~value))

(defmacro let ((param value & pairs) body)
  `(let- (~param ~value)
    ~(if (atom? pairs)
      body
      `(let ~pairs ~body))))

(defmacro -> (x & fs)
  (if (atom? fs)
    x
    (let (((operand & operators) & tl) fs)
      `(-> (~operand ~x ~@operators)
        ~@tl))))

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

(defun sum (lst)
  (foldl lst 0 +))

(defun reverse (lst)
  (foldl lst nil
    (lambda (acc x) (cons x acc))))

(defun map (lst f)
  (foldr lst nil
    (lambda (hd tl)
      (cons (f hd) tl))))

(defun filter (lst pred)
  (foldr lst nil
    (lambda (hd tl)
      (if (pred hd)
        (cons hd tl)
        tl))))

(defun inc (x) (+ 1 x))

(defun == (x y)
  (cond
    (and (atom? x) (= x y)) true))
    
