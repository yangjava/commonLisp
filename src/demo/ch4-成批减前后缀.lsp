;�������ּ�ǰ��׺����
(DEFUN C:ch4 ()
 (setq qh (getint "\n1--��ǰ׺,2--����׺,<1>"))
 (if (= qh nil)(setq qh 1))
 (princ "\nselect object:")
 (setq s (ssget))
 (princ "һ������ռ�����ֽڣ����ְ���������������")
 (setq nnn (getint "\n����Ҫ��������<1>:"))
 (if (= nnn nil)(setq nnn 1))
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
        (setq len0 (strlen hh) len1 (- len0 nnn))
        (if (= qh 1)(setq  str1 (substr hh (+ 1 nnn) len1)))
	(if (/= qh 1)(setq str1 (substr hh 1 len1)))
	(setq h1 (cons 1 str1))
        ;(if (= str "") (setq h1 h))
        (setq a (subst h1 h a))
        (entmod a)
        ))
      (setq k (+ k 1))
 )
)



 