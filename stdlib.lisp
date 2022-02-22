(def true 'true)
(def otherwise true)
(def nil '())
(def false nil)

(def list (lambda (&rest xs) xs))

(def foldr
  (lambda (lst z f)
    (cond
      (atom? lst) z
      otherwise (f
                  (head lst)
                  (foldr (tail lst) z f)))))

(def concat
  (lambda (&rest nested)
    (foldr
      nested
      nil
      (lambda (xs ys) (foldr xs ys cons)))))

(defmacro if (b x y)
  `(cond
      ~b ~x
      otherwise ~y))

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

(defun not (x)
  (if x
    false
    true))

(defun second (lst)
  (head (tail lst)))

(defmacro let- ((param value) body)
  `((lambda (~param) ~body) ~value))

(defmacro let ((param value &rest pairs) body)
  `(let- (~param ~value)
    ~(if (atom? pairs)
      body
      `(let ~pairs ~body))))

(defmacro -> (x &rest fs)
  (if (atom? fs)
    x
    (let (((operand &rest operators) &rest tl) fs)
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

(defun list-eq (xs ys)
  (if (atom? x)
    (atom? y)
    (let ((x &rest xs1) xs
          (y &rest ys1) ys)
      (and
        (== x y)
        (list-eq xs1 ys1)))))
  
(defun == (x y)
  (cond
    (and (atom? x) (atom? y))   (= x y)

    (and (not (atom? x))
         (not (atom? x)))       (list-eq x y)
    
    otherwise                   false))

(defun /= (x y)
  (not (== x y)))

(defun > (x y)
  (< y x))

(defun <= (x y)
  (or (== x y) (< x y)))

(defun >= (x y)
  (or (== x y) (> x y)))
