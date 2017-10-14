(defun readlh( / txt len n l-lh l-n txt1 txt_1 num1 num2 num) 
 (setq txt (getstring)) ;读入字符
 (setq txt (if txt txt "0"))
 (setq len (strlen txt)
       n 1
       l-lh '()
       l-n '()
 )
 (repeat len ;找出字符中所有","号并记录在表l-n中
  (setq txt1 (substr txt n 1))
  (if (= txt1 ",") 
   (setq l-n (cons (1- n) l-n))
  ) ;if end
  (setq n (1+ n))
 )  ;repeat end
 (setq  l-n (cons len l-n)
        l-n (reverse l-n)
        l-n (cons '-1 l-n)
        n (length l-n)
        m 0
 ) ;setq end
 (repeat (1- n) ;找出每个输入数值
  (setq n1 (+ 2 (nth m l-n))
        n2 (nth (1+ m) l-n)
        n2 (- n2 n1 -1)
        at (substr txt n1 n2)
        m (1+ m)
  ) ;setq end        
  (setq len1 (strlen at)
        num1  -1 ; "("位置
        num2 -1  ; ")"位置
        num 1)
  (repeat len1 
        (Setq txt_1 (substr at num 1)
        ) ;setq end
        (if (= txt_1 "(" ) (setq num1 num))
        (if (= txt_1 ")" ) (Setq num2 num))
        (setq num (1+ num))
   ) ;repeat end
   (if (= num1 -1) 
       (progn  ;没有找到"("号
        (setq at1 (atof at)
              at1 (* at1 scale)
              l-lh (cons at1 l-lh)
        ) ;setq end
       ) ;progn end 
       (progn ;有找到"("号
        (setq n_rep (atof (substr at 1 (1- num1)))
              at1    (atof (substr at (1+ num1) (1- num2)))
              at1    (* scale at1)
        ) ;setq end
        (repeat (fix n_rep)
         (setq l-lh (cons at1 l-lh))
        ) ;repeat end
       ) ;progn end
   ) ;if end
 ) ;repeat end
 (setq l-lh (reverse l-lh))
) ;defun end
;
;
(defun C:TAB()
 (inivar) ;初始化系统变量
 (princ "\n表格各列之间的距离(mm,请按1:1比例输入,从左到右):" )
 (setq scale (getvar "userr1"))
 (setq l_x (readlh))
 (princ "\n表格各行之间的距离(mm,请按1:1比例输入,从下到上):" )
 (setq l_y (readlh))
 (Setq len_x (length l_x)
       len_y (length l_y)
       lx 0
       ly 0
       n 0)
 (repeat len_x
     (setq lx (+ lx (nth n l_x))
           n (1+ n)
     ) ;setq end
 ) ;repeat end
 (Setq n 0)
 (repeat len_y
     (setq ly (+ ly (nth n l_y))
           n (1+ n)
     ) ;setq end
 ) ;repeat end
 (princ "\n表格插入点:")
 (command "insert" "border" "x" lx "y" ly "r" 0 pause)
 (setq p0 (getvar "lastpoint"))
 (command "erase" "l" "" )
 (setq l_x (cons '0 l_x)
       l_y (cons '0 l_y)
       len_x (length l_x)
       len_y (length l_y)
       n_x 0
       n_y 0
       dis 0
 ) ;setq end
 (repeat len_x
         (setq dis (+ dis (nth n_x l_x))
               n_x (1+ n_x)
               p1 (polar p0 0 dis)
               p2 (polar p1 pi09 ly)
         ) ;setq end
         (command "line" p1 p2 "")
 ) ;repeat end
 (setq dis 0)
 (repeat len_y
         (setq dis (+ dis (nth n_y l_y))
               n_y (1+ n_y)
               p1 (polar p0 pi09 dis)
               p2 (polar p1 0 lx)
         ) ;setq end
         (command "line" p1 p2 "")
 ) ;repeat end
 (resvar) ;还原系统变量
) ;defun end
           
