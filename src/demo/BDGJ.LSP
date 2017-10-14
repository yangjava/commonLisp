(VMON)

(defun mid(p1 p2 / x1 x2 y1 y2 x y)
  (setq x1 (nth 0 p1) y1 (nth 1 p1)
        x2 (nth 0 p2) y2 (nth 1 p2)
        x  (/ (+ x1 x2) 2)
        y  (/ (+ y1 y2) 2)
  )
  (setq p  (list x  y ))
)
(defun chgp1(p1 p2 / temp)
  (setq x1 (nth 0 p1) y1 (nth 1 p1)
        x2 (nth 0 p2) y2 (nth 1 p2)
  )
  (if (and (= x1 x2) (> y1 y2))  (setq tmp p1  p1  p2  p2  tmp))
  (if (> x1 x2)  (setq tmp p1  p1  p2  p2  tmp))
  (setq temp (list p1 p2))
)

(defun pt( p1 p2 ang dist / p3)
  (setq ang1 (angle p1 p2)
        ang  (+ ang ang1)
        p3   (polar p2 ang dist)
  )
)

(defun read_d (/ txt1 txt2 n nn nlen tmp error old)
 (setq loop T
       error nil
 )
 (if (null r_txt) (setq r_txt "10a250"))
 (setq old  r_txt)
 (while loop  ;while 1
   (princ  "\n输入直径与间距,I级钢用a,II级钢用b隔开直径与间距.")
   (princ  "\n如10a200,12b150,打N或n不注写直径与间距,回车用默认值.")
   (if error
     (princ "\n输入错误,请重新输入<")
     (princ "\n请输入直径与间距<")
   )
   (princ old)
   (setq r_txt (getstring ">:"))
   (if (= 0 (strlen r_txt)) (setq r_txt old))
   (if (= (strcase r_txt) "N")
     (setq  GJ_BZ  0
            loop   nil
     )
     (progn    ;p1
        (setq nlen (strlen r_txt))
        (setq nn 1)
        (while (< nn nlen)
           (setq tmp (substr r_txt nn 1))
           (if  (or (= tmp "a") (= tmp "b")) ;if2
               (setq n    nn
                     loop nil
                     nn   nlen   ;while find a or b,stop while
                     GJ_BZ 1
               );setq end
             (progn
               (setq nn (1+ nn))
               (if (= nn nlen)
                   (setq loop  T  ;can not find a or b
                         error T
                   )
               )
             )
           );if2 end
        );while end
     );p1 end
   );if1 end
 );while 1 end
 (if (= 1 GJ_BZ) ;if3
   (progn
     (if (= tmp "a") (setq gjdj 1))
     (if (= tmp "b") (setq gjdj 2))
     (setq  txt1 (substr r_txt 1 (1- n))
            txt2 (substr r_txt (1+ n))
     )
     (setq tlist (list txt1 txt2))
   )
 ) ;if3 end
)
(defun g_line(p1 p2 / d pi5/4 pi3/4 p3 p4 p5 p6)
  (setq p  (chgp1 p1 p2)
        p1 (car p)
        p2 (cadr p)
        d  (* 1.5 scale)
  )
  (setq oldlayr (getvar "CLAYER"))
  (command "layer" "m" "rei" "c" 1 "" "")
  (if (null gjdj) (setq gjdj (getint "\n钢筋等级(1/2)<1>:")))
  (if (null gjdj) (setq gjdj 1))
  (if (= 1 gjdj)
    (progn
      (setq p3 (pt p2 p1 pi27 d)
            p4 (pt p1 p3 pi27 d)
            p5 (pt p1 p2 pi09 d)
            p6 (pt p2 p5 pi09 d)
      )
      (command "pline" p4 "w" 0 "" p3 "a" p1 "l" p2 "a" p5 "l" p6 "")
    )
    (progn
      (setq pi54 (/ (* pi 5) 4)
            pi34 (/ (* pi 3) 4)
            p3 (pt p2 p1 pi54 d)
            p4 (pt p1 p2 pi34 d)
      )
      (command "pline" p3 "w" 0 ""  p1 p2 p4 "")
    )
  );if end
  (setvar "CLAYER" oldlayr)
)

(defun chgp(p1 p2 / d temp)
  (setq x1 (nth 0 p1) y1 (nth 1 p1)
        x2 (nth 0 p2) y2 (nth 1 p2)
        d (- (+ x2 y2) (+ x1 y1))
  )
  (if (< d 0.0) (setq temp p1   p1 p2  p2 temP)) ;if end
  (setq p (list p1 p2))
)

(defun bz( p1 p2  txt / ang1 ang2 ang p txt3 dist1
                dist txt_p txt txt1 txt2 ent txt_ht)
  (setq
        mid_p  (mid p1 p2)
        txt_ht (* 3 scale)
        p (chgp p1 p2)
        p1 (car p)
        p2 (cadr p)
        ang1 (angle p1 p2)
        ang2 (+ ang1 PI09)
        ang (/ (* 180 ang1) PI)
        dist (* 3 scale)
        txt_p (polar mid_p ang2 dist)
  );setq end
;  (setq txt (read_d))
  (if (= GJ_BZ 1)
    (setq
          txt1 (if (= gjdj 1) (strcat "%%130" (nth 0 txt)) (strcat "%%131" (nth 0 txt)) )
          txt2 (strcat "@" (nth 1 txt))
          txt1 (strcat txt1 txt2)
    )
  );if end
  (setq txt3 (getint "\n钢筋号:"))
  (setq ent (ssadd))
  (if (= GJ_BZ 1)(progn
      (if txt3
        (command "text" "j" "ml" txt_p txt_ht ang txt1)
        (command "text" "j" "m" txt_p txt_ht ang txt1)
      )
      (setq ent (ssadd (ssname (ssget  "L") 0) ent))
  ));if end
  (if txt3 (progn
      (setq big (* 2.5 scale))
      (setq dist1 (* 3.5 scale))
      (if (= GJ_BZ 1) (setq txt_p (polar txt_p  ang1 (- dist1))))
      (command "insert" "circle" txt_p  big "" ang txt3)
      (setq ent (ssadd (ssname (ssget  "L") 0) ent))
      (setq txt_p (polar txt_p ang1 (- big)))
  ))
  (if (> (sslength ent) 0) (progn
    (princ "\n标注位置合适吗?若不合适,指定另外一点:")
    (setq txt_p (list (nth 0 txt_p) (nth 1 txt_p)))
    (command "ucs" "o" txt_P)
    (command "move" ent "" "0,0" pause)
    (command "ucs" "")
;   (setq p2 (getpoint txt_p))
;   (if (null p2) (setq p2 txt_p))
;   (command p2)
  ))
) ;defun end

(defun C:bd(/  gjdj p1 p2 txt oer)
  (setvar "cmdecho" 0)
  (setvar "blipmode" 0)
  (setq oer *error* *error* clmerr)
  (princ "\nCopyright Y.F.Bao 1996.画板底钢筋,打bd开始本命令.")
  (if (null scale) (setq scale (getreal "\n输入比例:")))
  (setq p1 (getpoint "\n输入钢筋第一点:"))
  (setq p2 (getpoint p1 "   钢筋第二点:"))
  (setq txt (read_d))
  (g_line p1 p2)
  (bz p1 p2 txt)
  (setvar "blipmode" 1)
  (setvar "cmdecho" 1)
  (setq  *error* oer)
  (princ)
)

;;;画墙梁钢筋,能自动标注
(VMON)
(defun read_dst(nn / txt txt1 txt2 nlen nub  FIND )
 (setq ltext
  '("\n下端伸出距离(mm)<" "\n上端伸出距离(mm)<" "\n左端伸出距离(mm)<" "\n右端伸出距离(mm)<"
    "\n上端,下端伸出距离(mm)<"  "\n左端,右端伸出距离(mm)<")
 )
 (princ (nth nn ltext))
 (if (null rd_str)
   (progn
     (if (<= nn 3)  (setq rd_str "200")
                    (setq rd_str "200,300")
     )
   )
 )
 (setq old rd_str)
 (princ old)
 (setq rd_str  (getstring ">:"))
 (if (= 0 (strlen rd_str)) (setq rd_str old))
 (setq nlen (strlen rd_str))
 (if (or (= 4 nn) (= 5 nn))
   (progn
     (setq nub 1)
     (setq FIND nil)
     (while (< nub nlen)     ;find if there has ,
       (setq txt (substr rd_str nub 1))
       (if (= txt ",")
           (setq
                 txt1  (substr rd_str 1 (1- nub))
                 txt2  (substr rd_str (1+ nub))
                 nub   nlen
                 FIND  T
                 tlist (list txt1 txt2)
           )
       )
       (if (< nub nlen) (setq nub (1+ nub)))
     );while end
     (if (not FIND)
       (setq tlist (list rd_str "0")) ;there has not ,
     )
   );progn end
   (setq tlist (list rd_str "0"))
 );if end
)
;defun end




(DEFUN C:QL();/ pc  ent xc yc p1 p2 x1 x2 xc yc pc y1 y2 pt1 xc1 yc1
             ;   HOR TYPE  d1 d2 d ang1 ang2 ang3 ang4 dlist)
 (setvar "cmdecho" 0)
 (command "undo" "m")
 (setq oer *error* *error* clmerr)
 (if (null scale) (setq scale "\n输入比例:"))
 (command "layer" "m" "rei" "c" "1" "" "s" "0" "")
 (princ "\nCopyright Y.F.Bao 1996.画墙梁钢筋,打ql开始本命令.")
;(princ "\n对边墙梁,选中靠里面的墙梁线,对中梁,选中两根粱线(梁线不能是多义线).")
 (setq os (getvar "osmode"))
 (setvar "osmode" 512)
 (setq pc (osnap (getpoint "\n请选第一根梁线:") "nea"))
   (setq ent (entget (ssname (ssget pc) 0)))
   (if (= "LINE" (cdr (assoc 0 ent)))
     (setq
        p1   (cdr (assoc 10 ent))
        p2   (cdr (assoc 11 ent))
        x1 (nth 0 p1) y1 (nth 1 p1)
        x2 (nth 0 p2) y2 (nth 1 p2)
     );setq end
     (progn
       (princ "\n选中的梁线不是直线,不能运行本程序,再见!")
       (exit)
     )
   );if end
   (setq HOR 2)
   (if (= (fix y1) (fix y2)) (setq HOR 1)) ;it is horizen line
   (if (= (fix x1) (fix x2)) (setq HOR 0)) ;it is vertical line
   (if (= HOR 2)
     (progn
       (princ "\n选中的梁线不垂直或水平,不能运行本程序,再见!")
       (exit)
     )
   )
   (setq xc (nth 0 pc )
         yc (nth 1 pc ))
   (setq pt1 (getpoint "\n请选第二根粱线,对边墙梁选择钢筋起始位置:"))
   (setq tmp (osnap pt1 "nea"))
   (if (null tmp)
     (progn
       (if (= 1 HOR)   ;if1
         (progn
           (if (> (nth 1 pt1 ) yc) (setq TYPE 1)) ;up
           (if (< (nth 1 pt1 ) yc) (setq TYPE 2)) ;low
           (setq pt1 (list xc (nth 1 pt1)))
         );progn end
         (progn        ;it is ver
           (if (> (nth 0 pt1 ) xc) (setq TYPE 3)) ;right
           (if (< (nth 0 pt1 ) xc) (setq TYPE 4)) ;left
           (setq pt1 (list (nth 0 pt1) yc))
         )
       );if1 end
     );progn end
     (progn
       (setq pt1 tmp
             xct (nth 0 pt1 )
             yct (nth 1 pt1 )
       );setq end
       (if (= 1 HOR)
         (progn
           (setq TYPE 5)
           (if (< yc yct)(setq tmp pt1 pt1 pc  pc  tmp))
           (setq pt1 (list xc yct))
         );progn end
       );if end
       (if (= 0 HOR)
         (progn
           (setq TYPE 6)
           (if (> xc xct) (setq tmp pt1 pt1 pc  pc  tmp))
           (setq pt1 (list xct yc))
         );progn end
       );if end
     );progn end
   );if1 end
   (read_dst (1- TYPE))
   (setq dlist  tlist
         d1      (atof (nth 0 dlist))
         d2      (atof (nth 1 dlist))
         d      (+ d1 d2)
   );setq end
   (if (= 0 d2)
     (if (or (= 5 TYPE) (= 6 TYPE))
        (setq d2 d1
              d  (* 2 d)
        )
     );if end
   );if end
   (if (= 1 HOR)
       (setq ang1 pi09 ang2 pi27 ang3 pi0)
       (setq ang1 pi18 ang2 pi0  ang3 pi27)
   ) ;if end
   (if (or (= 5 TYPE) (= 6 TYPE))
     (setq p1 (polar pc  ang1 d1)
           p2 (polar pt1 ang2 d2)
     );setq end
     (progn
       (setq p1 pt1)
       (if (= 1 TYPE) (setq p2 (polar pc pi27 d)))
       (if (= 2 TYPE) (setq p2 (polar pc pi09 d)))
       (if (= 3 TYPE) (setq p2 (polar pc pi18 d)))
       (if (= 4 TYPE) (setq p2 (polar pc pi0  d)))
     );progn end
   );if end
   (setq p3 (polar p1  ang3 (* 3.0 scale))
         p4 (polar p2  ang3 (* 3.0 scale))
         wid (* 0.0 scale)
   );setq end
   (command "layer" "s" "rei" "")
   (command "pline" p3 "w" wid wid p1 p2 p4 "")
   (command "layer" "s" "0" "")
   (setq txt (read_d))
   (bz p1 p2 txt)
   (setq dimpt (getpoint "\n钢筋尺寸标注点(打回车则不注尺寸):"))
   (if (null dimpt)
     (princ)
     (progn
       (setvar "dimlfac" 1.0)
       (setvar "dimdli" 0)
       (setvar "dimexo" (* 2 scale))
       (setq d1 (rtos d1 2 0)
             d2 (rtos d2 2 0)
             d1 (atoi d1)
             d2 (atoi d2)
       )
       (if (or (= 5 TYPE) (= 6 TYPE))
         (progn
           (if (= 0 HOR)
             (command "dim" "hor" p1 pc dimpt d1  "hor" pt1 p2 dimpt d2  "e")
             (command "dim" "ver" p1 pc dimpt d1  "ver" pt1 p2 dimpt d2  "e")
           )
         )
         (progn
           (if (= 0 HOR)
             (command "dim" "hor"  pc p2 dimpt d1  "e")
             (command "dim" "ver"  pc p2 dimpt d1  "e")
           )
         )
       )
     )
   );if end
(setq  *error* oer)
(setvar "cmdecho" 1)
(princ)
) ;defun end

