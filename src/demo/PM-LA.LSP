(vmon)
;
(defun readlh( / txt len n l-lh l-n txt1 txt_1 num1 num2 num ) 
 (setq txt (getstring))
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

(defun c:pm-la()
        (setvar "cmdecho" 0)
        (princ "\n要打开的层名:")
        (setq lla (readlh)
              nl2 (length lla)
              ll '("0" "100" "101" "2" "3" "31" "310" "311" "410" "411" "46" "6" "60" "61" "62" "tk" "zgj" "fgj" "zgjh" "fgjh")
              nl1 (length ll)
              cla1 (nth 0 lla)
              index 0
        ) ;setq end
        (command "layer" "s"  cla1 "")
        (repeat nl1
         (setq cla (nth index ll)
                index (1+ index)
         )
         (if (not (= cla cla1)) (command "layer" "off" cla ""))
        );repeat end 
        (setq index 0)
        (repeat nl2
         (setq cla (nth index lla)
                index (1+ index)
         )
         (command "layer" "on" cla "")
        );repeat end 
)


