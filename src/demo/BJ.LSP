(VMON)
   (setq gjdj 1)
   (setq fh78 0 fh65 0 fh72 0 fh85 0 fh70 370)
   (setq bl (getvar "userr1"))
   (if (<= bl 1.0) (setvar "userr1" 1))
   (setq bl (getvar "USERR1") blx (/ bl 100.0))
   (Setq scale bl)
   (setq d0 (* bl 10) d1 (* bl 15) d2 (* bl 43) d3 (* bl 47))
   (setq d4 (* bl 15) d5 (* bl 25) d6 (* bl 35) d7 (* bl 51) d8 (* bl 37))
   (setvar "ltscale" bl)
   (setvar "DIMASZ" 0)
   (setvar "DIMCEN" (* bl 2))
   (setvar "DIMEXO" 0)
   (setvar "DIMDLI" (* bl 10))
   (setvar "DIMEXE" (* bl 2))
   (setvar "DIMTXT" (* bl 3))
   (setvar "DIMDLE" (* bl 2))
   (setvar "DIMTIH" 0)
   (setvar "DIMTOH" 0)
   (setvar "DIMTAD" 1)
   (setvar "DIMTIX" 1)
   (setvar "DIMSOXD" 0)
   (setvar "DIMTSZ" (* bl 0.8))
   (setvar "USERI1" 2)
   (setvar "USERI2" 0)


(defun TTK (/ oer lw t1 t2 t3 t4 t5 t6 b1 b2 p1 a cd ths elv)
   (setvar "CMDECHO" 0)
   (setq ths (getvar "thickness"))
   (setq elv (getvar "elevation"))
   (setq oer *error* *error* tkerr)
   (setq t1 1189.0 t2 841.0 t3 594.0 t4 419.0 t5 297.0 t6 209.0)
   (setq lw 0.5 b1 25.0 b2 10.0)
   (setq tzh1 (getvar "USERI1"))
   (setq tzh2 (getvar "USERI2"))
   (setq aaa1 
        (strcat "A" (rtos tzh1))
   )
   (if (/= tzh2 0) 
       (setq aaa1 (strcat aaa1 "+"))
   )
   (princ "\n输入图纸号<")
   (princ aaa1)
   (setq a (getstring ">:"))
;   (setq a (getstring "\n输入图纸号 <A2>:"))
   (if (eq a "") (setq a aaa1))
   (if (= (substr a 3 1) "+") 
       (setvar "USERI2" 1)
       (setvar "USERI2" 0)
   )
   (blch)
   (setq t1 (* t1 bl) t2 (* t2 bl) t3 (* t3 bl) t4 (* t4 bl) t5 (* t5 bl) t6 (* t6 bl))
   (if (and (= (substr a 3 1) "+") (= (strlen a) 3)) (setq cd nil) (setq cd t))
   (setq a (atoi (substr a 2 1)))
   (setvar "USERI1" a)
   (if cd (progn
          (cond ((= a 0) (setq p1 (list t1 t2)))
                ((= a 1) (setq p1 (list t2 t3)))
                ((= a 2) (setq p1 (list t3 t4)))
                ((= a 3) (setq p1 (list t4 t5) b2 5.0))
                ((= a 4) (setq p1 (list t6 t5) b2 5.0))
          )
          )
       (progn
       (cond ((= a 1) (setq p1 (list t1 t3)))
             ((= a 2) (setq p1 (list t2 t4)))
             ((= a 3) (setq p1 (list t3 t5) b2 5.0))
             ((= a 4) (setq p1 (list t5 t5) b2 5.0))
      )
      )
   )
   (command "regen")
   (setvar "elevation" 0)
   (setvar "thickness" 0)
   (drawline)
   (setvar "elevation" elv)
   (setvar "thickness" ths)
   (setvar "cmdecho" 1)
   (setq *error* oer)
   (princ)
)

(defun blch (/ x)
   (princ "\n输入图纸比例 1: <")
   (princ (fix bl))
   (setq x (getreal "> "))
   (if x (setq bl x blx (/ bl 100)))
   (setq d0 (* bl 10) d1 (* bl 15) d2 (* bl 43) d3 (* bl 47))
   (setq d4 (* bl 15) d5 (* bl 25) d6 (* bl 35) d7 (* bl 51) d8 (* bl 37))
   (setvar "USERR1" bl)
   (setvar "ltscale" bl)
   (setvar "DIMASZ" 0)
   (setvar "DIMCEN" (* bl 2))
   (setvar "DIMEXO" 0)
   (setvar "DIMDLI" (* bl 10))
   (setvar "DIMEXE" (* bl 2))
   (setvar "DIMTXT" (* bl 3))
   (setvar "DIMDLE" (* bl 2))
   (setvar "DIMTIH" 0)
   (setvar "DIMTOH" 0)
   (setvar "DIMTAD" 1)
   (setvar "DIMTIX" 1)
   (setvar "DIMSOXD" 0)
   (setvar "DIMTSZ" (* bl 0.8))
   (princ)
)

(defun drawline (/ ll lr ur ul ll0 lr0 ur0 ul0)
   (setq b1 (* b1 bl) b2 (* b2 bl) lw (* lw bl))
   (setq ll (list 0 0) lr (list (car p1) 0) ur p1 ul (list 0 (cadr p1)))
   (setq ll0 (list b1 b2) lr0 (list (- (car p1) b2) b2) ur0 (list (- (car p1) b2) (- (cadr p1) b2)) ul0 (list b1 (- (cadr p1) b2)))
   (command "limits" ll ur)
   (command "zoom" "a")
   (command "pline" ll "w" "0" "" lr ur ul "c")
   (command "pline" ll0 "w" lw "" lr0 ur0 ul0 "c")
   (command "layer" "m" "tk" "")
   (command "insert" "acad00" lr0 blx "" "0")
   (command "insert" "acad01" ul0 blx "" "0")
   (command "layer" "s" "0" "")
)

;-------------------------------------
; 
;-----------------------
(DEFUN C:TK1()
 (setq oer *error* *error* clmerr)
  (SETQ P1 (getpoint "\n输入当前图的左下点:")
        P2 (getpoint "\n输入当前图的右上点:") 
  )
  (COMMAND "WBLOCK" "TEMP" "" p1 "C" P1 P2 "")
  (TTK)
  (SETQ SCALE (GETVAR "USERR1"))
  (COMMAND "INSERT" "TEMP" P1 SCALE SCALE 0)
  (COMMAND "EXPLODE" "L")
  (COMMAND "SH" "DEL TEMP.DWG")
  (command "zoom" "e")
) ;defun end
;--------------------------------- 
(DEFUN READT(nn / temp txt txt1 txt2 n k index)
;----------------------------------
 (setq ltext 
  '("\n直径,间距:" "\n左端,右端伸出距离(米):" "\n下端,上端伸出距离(米):") 
 )
 (princ (nth nn ltext))
 (setq temp T txt (getstring))
 (while temp
  (setq k 1  index T nlen  (strlen txt))
  (while index
     (if (= (substr txt k 1) ",")        
         (setq index nil)
         (progn
          (if (<= k nlen)
             (setq k (1+ k)) (setq index nil)
          ) ;if end
         ) ;progn end
    ) ;if end
  )  ;while end
  (if (< k nlen) 
   (setq temp nil)
   (progn
    (princ (strcat "\n输入错误,请再输入 " (nth nn ltext)))
    (princ (nth nn ltext))
    (setq txt (getstring))
   ) ;progn end
  ) ;if end
 ) ;while end  
 (setq nlen (strlen txt)
       n 1 temp T)
 (while temp
  (setq txt1 (substr txt n 1))
  (if (= txt1 ",") (Setq temp nil) (setq n (1+ n)))
 ) ;while end
 (setq txt1 (substr txt 1 (1- n))
       txt2 (substr txt (1+ n) (- nlen n))
 )
 (setq tlist (list txt1 txt2))
;------------------
) ;defun end
;------------------------- 
(DEFUN C:BDGJ()
;-------------------------
(setvar "orthomode" 1) 
 (setq oer *error* *error* clmerr)
(command "layer" "m" "rei" "c" "1" "" "s" "0" "")
(setvar "cmdecho" 0)
(while (setq p1 (getpoint "\n第一点:"))
 (princ "\n第二点:") 
 (command "line" p1 pause "")
 (setq p2 (getvar "lastpoint"))
 (command "erase" "l" "")
 (setq x1 (nth 0 p1) y1 (nth 1 p1)
       x2 (nth 0 p2) y2 (nth 1 p2)
       d (- (+ x2 y2) (+ x1 y1))
 ) ;setq end
 (if (< d 0.0) 
     (setq temp p1 
           p1 p2 
           p2 temP)
 ) ;if end
 (if (= y1 y2)
   (setq d1 (* 2 scale)
         d2  d1
         p4 (polar p1 pi09 d1)
         p3 (polar p4 pi0 d2)
         p5 (polar p2 pi09 d1)
         p6 (polar p5 pi18 d2)
         ang1 pi09
         ang2 0
         ang3 pi27
         ang4 0
         ang5 (* 0.25 pi)
         ang6 (* 0.75 pi)
   ) ;setq end
   (setq d1 (* 2 scale)
         d2 d1
         p4 (polar p1 pi18 d1)
         p3 (polar p4 pi09 d2)
         p5 (polar p2 pi18 d1)
         p6 (polar p5 pi27 d2)
         ang1 pi18
         ang2 pi09
         ang3 pi0
         ang4 90
         ang5 (* 0.75 pi)
         ang6 (* 1.25 pi)
   ) ;setq end
 ) ;if end  
 (setq wid (* 0.0 scale)
       htext (* 3 scale)
       htxt1 (* 2.5 scale)
 )
 (setq txt (readt 0)
       txt1 (if (= gjdj 1) (strcat "%%133" (nth 0 txt)) (strcat "%%132" (nth 0 txt)) )
       txt2 (strcat "@" (nth 1 txt))
       txt3 (getstring "\n钢筋号:")
       pc1 (polar p1 ang1 (* 2.0 scale))  
       pc1 (polar pc1 ang2 (* 0.3 (abs d)))
       pt1 (polar pc1 ang2 (* 3.0 scale))
       pc1 (polar pc1 ang1 (* 2 scale))
       pt2 (polar pt1 ang3 (* 7.0 scale))
       r (* 2.0 scale)   
 )     
 (command "layer" "s" "rei" "")
 (if (= gjdj 1) 
  (command "pline" p3 "w" wid wid p4 p1 p2 p5 p6 "")
  (progn
    (setq p3 (polar p1 ang5 (* 2.0 scale))
          p4 (polar p2 ang6 (* 2.0 scale))
    ) ;setq end
    (command "pline" p3 "w" wid wid p1 p2 p4 "")
  ) ;progn end
 )   ;if end
 (command "layer" "s" "0" "")
 (command "circle" pc1 r)
 (command "text" "j" "m" pc1 htxt1 ang4 txt3)
 (setq txt1 (strcat txt1 txt2))
 (command "text" pt1 htext ang4 txt1)
) ;while end
;---------------------
(setvar "cmdecho" 1)
) ;defun end
;---------------------   
 
(DEFUN C:QLGJ()
 (command "layer" "m" "rei" "c" "1" "" "s" "0" "")
 (setvar "cmdecho" 0)
 (setq oer *error* *error* clmerr)
 (princ "\n请务必选中轴线,否则绘图会有误差!")
 (while (setq ent (entsel "\n墙梁位置:"))
        (setq pc (nth 1 ent)
              ent (car ent)
              ent (entget ent)
              xc (nth 0 pc) 
              yc (nth 1 pc)
              p1 (cdr (assoc 10 ent))
              p2 (cdr (assoc 11 ent))
              x1 (nth 0 p1) y1 (nth 1 p1)
              x2 (nth 0 p2) y2 (nth 1 p2)
              xc (if (= (fix y1) (fix y2)) xc x1)
              yc (if (= (fix x1) (fix x2)) yc y1)
              pc (list xc yc)
        ) ;setq end      
;---------         
;        (princ "\nent:") (princ ent)
;        (princ "\nx1,x2:") (princ x1) (princ " ") (princ x2)
;        (princ "\ny1,y2:") (princ y1) (princ " ") (princ y2)
;-----------         
        (setq dlist (if (= (fix x1) (fix x2)) (readt 1) (readt 2))
              d1  (* (atof (nth 0 dlist)) 1000)
              d2  (* (atof (nth 1 dlist)) 1000)
              d (+ d1 d2)  
         ) ;setq end
;---------         
;        (princ "\nx1,x2:") (princ x1) (princ x2)
;        (princ "\ny1,y2:") (princ y1) (princ y2)
;-----------         
         (if (= (fix y1) (fix y2))
             (setq ang1 pi27 ang2 pi09 ang3 pi0)
             (setq ang1 pi18 ang2 pi0 ang3 pi27)
         ) ;if end
         (setq p1 (polar pc ang1 d1)
               p2 (polar p1 ang2 (+ d1 d2))
               p3 (polar p1 ang3 (* 3.0 scale))
               p4 (polar p2 ang3 (* 3.0 scale))
               wid (* 0.0 scale)
         )
         (command "layer" "s" "rei" "")
         (command "pline" p3 "w" wid wid p1 p2 p4 "")
         (command "layer" "s" "0" "")
         (if (< d 0.0) 
                (setq temp p1 
                      p1 p2 
                      p2 temP)
         ) ;if end
        (if (= (fix x1) (fix x2))
          (setq ang1 pi09
                ang2 0
                ang3 pi27
                ang4 0
                di1 (polar p1 pi09 (* 4.0 scale))
                di2 (polar di1 pi0  d1)
                di3 (polar di2 pi0  d2)
                dp (polar di1 pi09 (* 8 scale))
          ) ;setq end
          (Setq ang1 pi18
                ang2 pi09
                ang3 pi0
                ang4 90
                di1 (polar p1 pi18 (* 4.0 scale))
                di2 (polar di1 pi09 d1)
                di3 (polar di2 pi09 d2)
                dp (polar di1 pi18 (* 8 scale))
          ) ;setq end
        ) ;if end  
        (setq wid (* 0.0 scale)
                htext (* 3 scale)
                htxt1 (* 2.5 scale)
        )
        (setq txt (readt 0)
         txt1 (IF (= gjdj 1) (strcat "%%133" (nth 0 txt)) (strcat "%%132" (nth 0 txt)) )
                txt2 (strcat "@" (nth 1 txt))
                txt3 (getstring "\n钢筋号:")
                pc1 (polar p1 ang1 (* 2.0 scale))  
                pc1 (polar pc1 ang2 (* 0.3 (abs d)))
                pt1 (polar pc1 ang2 (* 3.0 scale))
                pc1 (polar pc1 ang1 (* 2 scale))
                pt2 (polar pt1 ang3 (* 7.0 scale))
                r (* 2.0 scale)   
        )     
        (command "circle" pc1 r)
        (command "text" "j" "m" pc1 htxt1 ang4 txt3)
        (setq txt1 (strcat txt1 txt2))
        (command "text" pt1 htext ang4 txt1)
        (setvar "dimlfac" 1.0)
        (setvar "dimdli" 0)
        (if (= (fix x1) (fix x2))
         (command "dim" "hor" di1 di2 dp "" "con" di3 "" "e")
         (command "dim" "ver" di1 di2 dp "" "con" di3 "" "e")
        ) ;if end
 ) ;while end
(setvar "cmdecho" 1)
) ;defun end
;
