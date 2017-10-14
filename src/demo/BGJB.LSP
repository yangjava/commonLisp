(vmon)
;
(defun readlh(txt / txt len n l-lh l-n txt1 txt_1 num1 num2 num ) 
 (setq txt (if txt txt "0"))
 (setq len (strlen txt)
       n 1
       l-lh '()
       l-n '()
;       scale (getvar "userr1")
 )
 (repeat len
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
 (repeat (1- n)
  (setq n1 (+ 2 (nth m l-n))
        n2 (nth (1+ m) l-n)
        n2 (- n2 n1 -1)
        at (substr txt n1 n2)
        m (1+ m)
  ) ;setq end        
  (setq len1 (strlen at)
        num1  -1
        num2 -1
        num 1)
  (repeat len1
        (Setq txt_1 (substr at num 1)
        ) ;setq end
        (if (= txt_1 "(" ) (setq num1 num))
        (if (= txt_1 ")" ) (Setq num2 num))
        (setq num (1+ num))
   ) ;repeat end
   (if (= num1 -1) 
       (progn 
        (setq 
              l-lh (cons at l-lh)
        ) ;setq end
       ) ;progn end 
       (progn
        (setq n_rep (atof (substr at 1 (1- num1)))
              at1    (substr at (1+ num1) (1- num2))
        ) ;setq end
        (repeat (fix n_rep)
         (setq l-lh (cons at1 l-lh))
        ) ;repeat end
       ) ;progn end
   ) ;if end
 ) ;repeat end
 (setq l-lh (reverse l-lh))
) ;defun end


(defun c:bgjb()
; (setvar "cmdecho" 0)
 (setq fn (getstring "\n数据文件名:")
       f1 (open fn "r")
       scale (getvar "userr1")
       b (*  75 scale)
       h (* 15 7 scale)
       os (getvar "osmode")
 ) ;setq end
 (setvar "osmode" 0)
 (princ "\n插入点:")
 (command "insert" "border" "x" b "y" h "r" 0 pause)
 (command "erase" "l" "")
 (setq p1 (getvar "lastpoint")
       p1 (polar p1 pi09 (* 15 7 scale))
       p2 (polar p1 0 b)
       p3 (polar p1 pi27 (* 7 scale))
       p4 (polar p3 0 b)
       pt0 (polar p3 pi09 (* 1.5 scale))
       pt1 (polar pt0 0 (* 4.5 scale))
       pt2 (polar pt0 0 (* 18.5 scale))
       pt3 (polar pt0 0 (* 29.5 scale))
       pt4 (polar pt0 0 (* 51.0 scale))
       pt5 (polar pt0 0 (* 66.0 scale))
       th (* scale 4)
 );setq end
 (command "line" p1 p2 "")
 (command "line" p3 p4 "")
 (command "text" pt1 th 0 "编号")
 (command "text" pt2 th 0 "类型")
 (command "text" pt3 th 0 "规格")
 (command "text" pt4 th 0 "L1")
 (command "text" pt5 th 0 "L2")
 (while (setq txt (read-line f1))
        (setq ll (readlh txt)
              index 0
              p3 (polar p3 pi27 (* 7 scale))
              p4 (polar p3 0 (* 75 scale))
              pt1 (polar p3 pi09 (* 3.5 scale))
              pt1 (polar pt1 0 (* 7.5 scale))
              pt0 (polar p3 pi09 (* 1.5 scale))
              pt2 (polar pt0 0 (* 18.5 scale))
              pt3 (polar pt0 0 (* 29.5 scale))
              pt4 (polar pt0 0 (* 51.0 scale))
              pt5 (polar pt0 0 (* 66.0 scale))
              th (* scale 3)
              txt1 (nth 0 ll)
              txt2 (nth 1 ll)
              txt3 (nth 2 ll)
              txt4 (nth 3 ll)
              txt5 (if (> (length ll) 4) (nth 4 ll) " ")
              pr (* 2.1 scale)
        ) ;setq end
        (command "line" p3 p4 "")
        (command "text" "j" "m" pt1 th 0 txt1)
        (command "circle" pt1 pr)
        (command "text" pt2 th 0 txt2)
        (command "text" pt3 th 0 txt3)
        (command "text" pt4 th 0 txt4)
        (command "text" pt5 th 0 txt5)
 ) ;while end
 (setq lb '(0 15 10 20 15 15)
       index 0
 ) ;setq end
 (repeat 6
  (setq p1 (polar p1 0 (* scale (nth index lb)))
         p3 (polar p3 0 (* scale (nth index lb)))
         index (1+ index)
  ) ;setq end
  (command "line" p1 p3 "")
 ) ;repeat end
 (setvar "cmdecho" 0)
 (setvar "osmode" os)
) ;defun end


              



       

