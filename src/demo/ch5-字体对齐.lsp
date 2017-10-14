;改pk字符两头对齐字体程序
(DEFUN C:ch5 ()
 (princ "\nselect object:")
 (setq s (ssget))
 (setq str (getint "\n0=左对正；1=居中对正；2=右对正<0>:"))
 (if (= str nil)(setq str 0))
 (setq h1 (cons 72 str))
 (setq n (sslength s))
 (setq k 0 )
 (while (< k n) 
      (setq name (ssname s k))
      (setq a (entget name))
      (setq t1 (assoc '0 a))
      (setq t1 (cdr t1))
      (if (= t1 "TEXT") (PROGN
        (setq h (assoc '72 a))
        (if (= str "") (setq h1 h))
        (setq a (subst h1 h a))
        (entmod a)
        ))
      (setq k (+ k 1))
 )
)
