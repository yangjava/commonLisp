;һ���Ӽ�ǰ����׺�ĳ���
(DEFUN C:ch3 ()
 (setq qh (getint "\n1--��ǰ׺,2--�Ӻ�׺,<1>"))
 (if (= qh nil)(setq qh 1))
 (princ "\nselect object:")
 (setq s (ssget))
 (setq str (getstring "\n����Ҫ�ӵ���:"))
 (setq n (sslength s))
 (setq k 0 )
 (while (< k n) 
      (setq name (ssname s k))
      (setq a (entget name))
      (setq t1 (assoc '0 a))
      (setq t1 (cdr t1))
      (if (= t1 "TEXT") (PROGN
        (setq h (assoc '1 a))
	(setq hh (cdr h))
        (if (= qh 1)(setq  str1 (strcat str hh)))
	(if (/= qh 1)(setq str1 (strcat hh str)))
	(setq h1 (cons 1 str1))
        ;(if (= str "") (setq h1 h))
        (setq a (subst h1 h a))
        (entmod a)
        ))
      (setq k (+ k 1))
 )
)



 