(defun ziti(txt / old a)
  (if (= (substr txt 1 1) ",")
    (progn
      (if (= (strlen txt) 1)
        (setq old_sty (getstring "\n输新字体:"))
        (setq old_sty (substr txt 2))
      )
      (setq a (tblsearch "style" old_sty))
      (if (null a) (progn
         (princ "\n")
         (princ old_sty)
         (princ "字体名没有建立,")
         (setq old_sty (getvar "textstyle"))
         (exit)
      ));if end
      (setq a 1)
    );progn end
    (setq a 0)
  );if end
 )

(DEFUN NTHL(N0 L0)(CDR (ASSOC N0 L0)))

(DEFUN pm_len(C0 H0 / E0 L0)
 (IF (= 32 (ASCII C0))(SETQ C0 (STRCAT "P" (SUBSTR C0 2))) )
 (IF (/= "" C0)(PROGN
  (COMMAND "TEXT" "R" (TRANS '(0 0) 0 1) H0 0 C0)
  (SETQ E0 (ENTLAST) L0 (ENTGET E0))
  (SETQ H0 (+ (/ H0 4) (DISTANCE (NTHL 10 L0) (NTHL 11 L0))))
  (ENTDEL E0) ) )
 H0
)
;画剖面号
(defun C:PM( / p0 txt_pm sty symb wid_scl lx ly p1 p2 p_txt h_txt oldcol txt
               ch_ds oer)
 (setvar "cmdecho" 0)
 (setvar "blipmode" 0)
 (setq oer  *error*  *error*  clmerr)
 (if (null old_sty) (setq old_sty (getvar "textstyle")))
 (princ "\n剖面号\\字体,(")
 (princ old_sty)
 (setq txt_pm (getstring ")<1-1>:"))
 (if (/=  "" txt_pm)(progn
    (setq a (ziti txt_pm))
    (if (= a 1) (setq txt_pm (getstring "\n剖面号:")))
 )
 (setq txt_pm "1-1")
 )
 (setq
       ly (* scale 6)
       lx (pm_len txt_pm ly)
       lx (+ lx (* scale 3))
       ly (* scale 8)
 );setq end
 (princ "\n剖面插入点:")
 (command "insert" "border" "x" lx "y" ly "r" 0 pause)
 (command "erase" "l" "")
 (setq p0 (getvar "lastpoint")
       p1 p0
       p2 (polar p1 0 lx)
       p_txt (polar p0 0 (/ lx 2.0))
       p_txt (polar p_txt pi09 (* 1 scale))
       h_txt (* 6 scale)
  ) ;setq end
 (command "layer" "m" "rei" "c" "y" "" "")
 (command "pline" p1 "w" (* 0.0 scale) "" p2 "")
 (command "layer" "s" "0" "")
 (setq p1 (list (car p1) (- (cadr p1) (* 1 scale)))
       p2 (list (car p2) (- (cadr p2) (* 1 scale)))
 )
 (command "line" p1 p2 "")
 (setq old (getvar "textstyle"))
 (setvar "textstyle" old_sty)
 (command "text" "j" "c" p_txt h_txt 0 txt_pm)
 (setvar "textstyle"  old)
 (setq  *error*  oer)
 (princ)
) ;defun end


;画折断符号
(defun C:zdx( / p1 p2 x1 x2 y1 y2 mid_p ang dis PI2
               mid_up mid_lp p3 p4 a os tmp)
(setvar "cmdecho" 0)
(setvar "blipmode" 0)
(setq olderr  *error*             ; Initialize variables
      *error* clmerr)
(princ "\n画折断线,打zdx开始本命令.")
(if (null scale) (setq scale (getint "\n请输入比例:")))
(while (null p1) (setq p1 (getpoint "\n第一点:")))
(princ "\n折断线高度(H)<")
(if (or (= 0 len ) (null len)) (setq len (* 4 scale)))
(princ len)
(initget 128)
(setq p2 (getpoint p1 ">\\第二点:"))
(setq os (getvar "osmode"))
(setvar "osmode" 0)
(if (not (listp p2))(progn
 (if (= (strcase (substr p2 1 1)) "H")
   (progn
     (if (= (strlen p2) 1)
       (progn
          (initget 2)
          (setq len (getdist "\n折断线高度:"))
       );progn end
       (setq len (atof (substr p2 2)))
     );if end
     (setq p2 (getpoint p1 "第二点:"))
   );progn end
 );if end
));if end
(setq x1 (car p1)
      y1 (cadr p1)
      x2 (car p2)
      y2 (cadr p2)
   );setq end
(setq mid_p (list (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0)))
(setq ang (angle p1 p2)
      dis (/ (distance p1 p2) 2.0)
  );setq end
(setq PI2 (/ PI 2)
      rept 1)
(while (= rept 1)
  (setq mid_up (polar mid_p (+ PI2 ang) len)
        mid_lp (polar mid_p (- ang PI2) len)
   );setq end
  (setq p3 (polar p1 ang (- dis (* 0.40 len)))
        p4 (polar p1 ang (+ dis (* 0.40 len)))
    );setq end
  (grdraw p1 p3 1 1)
  (grdraw p3 mid_up 1 1)
  (grdraw mid_up mid_lp 1 1)
  (grdraw mid_lp p4 1 1)
  (grdraw p4 p2 1 1)
  (setq a (getstring "\n高度合适吗?(Y)\\新折断线高度<y>:"))
  (if (or (= (strcase a) "Y") (= "" a))
    (setq rept 0)
    (progn
      (redraw)
      (setq len (atof a))
    );progn end
  );if end
);while end
(redraw)
(setq tmp  len
      p1 (polar p1 ang (- tmp))
      p2 (polar p2 ang tmp)
)
(command "pline" p1 "w" 0 "" p3 mid_up mid_lp p4 p2 "")
(setvar "cmdecho" 1)
(setvar "osmode" os)
(setq *error* olderr)
)
(defun w_txt( hor p1 p2 txt / x1 x2 y1 y2 mid_p txt_p old)
  (setq x1 (nth 0 p1)
        y1 (nth 1 p1)
        x2 (nth 0 p2)
        y2 (nth 1 p2)
  )
  (setq x1     (/ (+ x1 x2) 2)
        y1     (/ (+ y1 y2) 2)
        mid_p  (list x1 y1)
        txt_ht (* 4 scale)
        dist   (* 3 scale)
        dist1  (* 2 scale)
  )
  (if (= hor 1) (setq txt_p (polar mid_p PI09 dist)))
  (if (= hor 3) (setq txt_p (polar mid_p PI09 (- dist))))
  (if (= hor 0) (setq txt_p (polar mid_p 0 dist1)))
  (if (= hor 2) (setq txt_p (polar mid_p PI18 dist1)))
  (setq old (getvar "textstyle"))
  (setvar "textstyle" pqx_sty)
  (command "text" "j" "m" txt_p txt_ht 0 txt)
  (setvar "textstyle" old)
)

(defun C:pqx(/ p1 p2 a rept x1 x2 y1 y2 hor p3 p4 len temp)
  (princ "\n画剖切符号,打pqx开始本命令.")
  (setq olderr *error*
        *error* clmerr)
  (setvar "cmdecho" 0)
  (setvar "orthomode" 1)
  (setvar "blipmode" 0)
  (if (null scale)
     (setq scale (getint "\n请输入比例:"))
     (progn
       (princ "\n现在的比例为:")
       (princ scale)
     )
  )
  (if (null pqx_sty) (setq pqx_sty (getvar "textstyle")))
  (princ "\n字体.(")
  (princ pqx_sty)
  (initget 129)
  (setq p1 (getpoint ")\\第一点:"))
  (if (not (listp p1))
    (progn
      (if (= (substr p1 1 1) ".") ;if2 begin
        (progn
          (if (= (strlen p1) 1)
            (setq pqx_sty (getstring "\n输新字体:"))
            (setq pqx_sty (substr p1 2))
          )
          (setq a (tblsearch "style" pqx_sty))
          (if (null a) (progn
             (princ "\n")
             (princ pqx_sty)
             (princ "字体名没有建立.")
             (setq pqx_sty (getvar "textstyle"))
             (exit)
          ));if end
          (setq p1 (getpoint "\n第一点:"))
        );progn end
      );if2 end
    );progn end
  );if end
  (setq rept 1)
  (while (= rept 1)
    (setq p2 (getpoint p1 "\n第二点:"))
    (setq x1 (nth 0 p1)
          y1 (nth 1 p1)
          x2 (nth 0 p2)
          y2 (nth 1 p2)
    )
    (if (= x1 x2) (setq hor 0))
    (if (= y1 y2) (setq hor 1))
    (if (> (+ x1 y1) (+ x2 y2)) ;p1 must be left or low point
      (setq temp p1
            p1   p2
            p2   temp
      )
    )
    (setq
          len (* scale 6)
    )
    (if (= 0 hor)
      (progn
        (setq p3 (polar p1 PI09 (- len)))
        (setq p4 (polar p2 PI09 len))
      )
      (progn
        (setq p3 (polar p1 0 (- len)))
        (setq p4 (polar p2 0 len))
      )
    );if end
    (grdraw  p1 p3 2 1)
    (grdraw  p2 p4 2 1)
    (if (= 1 hor) (princ "\n默认剖切号写在剖切线上方,若在下方,在剖切号前加-,如-A"))
    (if (= 0 hor) (princ "\n默认剖切号写在剖切线右方,若在左方,在剖切号前加-,如-A"))
    (initget 129)
    (setq txt (getpoint "\n剖切线位置合适吗?(不合适重输第一点)\\输入剖切号:"))
    (if (listp txt)
       (setq rept 1 p1 txt p2 nil)
       (setq rept 0)
    )
  );while end
  (redraw)
  (command "layer" "m" "rei" "c" "y" "" "")
  (command "line" p1 p3 "")
  (command "line" p2 p4 "")
  (command "layer" "s" "0" "")
; (setq txt (getstring "\n输入剖切号:"))
  (if (= "-" (substr txt 1 1))
    (progn
      (setq txt (substr txt 2))
      (if (= hor 0) (setq hor 2))
      (if (= hor 1) (setq hor 3))
    )
  )
  (w_txt hor p1 p3 txt)
  (w_txt hor p2 p4 txt)
  (setq *error* olderr)
  (princ)
)

;画钢筋断面
(defun C:gdm(  / p1 p2 p3 dis nn n dis subdis r1 r2 r3)
  (setvar "cmdecho" 0)
  (setvar "blipmode" 1)
  (setq r1 0)
  (princ "\n钢筋断面半径<")
  (princ (* 1.0 scale))
  (setq r2 (getreal ">:"))
  (if (null r2) (setq r2 (* 1.0 scale)))
  (setq r3 (/ r2 2))
  (princ "\n第一根钢筋位置:")
  (command "insert" "C_border" "x" r3 "y" r3 "r" 0 pause)
  (setq p1  (getvar "lastpoint"))
  (princ "\n最后一根钢筋位置:")
  (command "move" "l" "" p1  pause)
; (command "insert" "C_border" "x" r3 "y" r3 "r" 0 pause)
  (command "erase" "l" "")
  (setq p2  (getvar "lastpoint"))
  (setq n  (getint "\n中间几根钢筋<0>:"))
  (if (null n) (setq n 0))
  (setq n (1+ n))
  (setq ang    (angle p1 p2)
        dis    (distance p1 p2 )
        subdis (/ dis n)
  );setq end
  (setq nn 0)
  (setq oldcol (getvar "cecolor"))
  (command "color" "w")
  (while (<= nn  n)
    (setq p3 (polar p1 ang (* nn subdis)))
    (command "donut" r1 r2 p3 "")
    (setq nn (1+ nn))
  )
  (command "color" oldcol)
  (setvar "cmdecho" 1)
)



;;;;改变多义线或直线的宽度
(defun C:pd(/ p l n old_wd  wid ent  e )
  (setvar "cmdecho" 0)
  (command "undo" "m")
  (setq olderr  *error*             ; Initialize variables
         *error* clmerr)
  (setq p (ssget))
  (setq l 0 n(sslength p))
  (setq old_wd (* 0.40 bl))
  (princ "\nCopyright (C) (R) Y.F.BAO 1996.打pd运行此程序.")
  (princ "\n所有直线与多义线的统一宽度<")
  (princ old_wd)
  (setq wid (getreal ">:"))
  (if (null wid) (setq wid old_wd))
  (while (< l n)
    (setq ent (ssname p l))
    (setq attrb (cdr (assoc 0 (setq e (entget ent)))))
    (if (or (= "ARC" attrb) (= "LINE" attrb))
;  (if (= "LINE" attrb)
      (command "pedit" ent ""  "w"  wid "")
    )
    (if (= "POLYLINE" attrb)
      (command "pedit" ent "w" wid "")
    )
    (setq l (1+ l))
  );while end
  (command "redraw")
  (setq *error* olderr)
  (setvar "cmdecho" 1)
)


;等分两点间距离
(defun C:div( / p1 p2 p3 dis nn n dis subdis)
  (setq olderr  *error*             ; Initialize variables
         *error* clmerr)
  (setvar "cmdecho" 0)
  (setq oldpd (getvar "pdmode"))
  (setq p1 (getpoint "\n第一点:"))
  (setq p2 (getpoint p1 "\n第二点:"))
  (grdraw p1 p2 1 1)
  (setq n  (getint "\n等分为几部分<2>:"))
  (if (null n) (setq n 2))
  (setq ang    (angle p1 p2)
        dis    (distance p1 p2 )
        subdis (/ dis n)
  );setq end
  (setq nn 1)
  (setvar "pdmode" 2)
  (while (< nn n)
    (setq p3 (polar p1 ang (* nn subdis)))
    (command "point" p3 )
    (setq nn (1+ nn))
  )
  (setvar "pdmode" oldpd)
  (setq *error olderr)
  (princ)
)


(defun list_str(p_list / a b tmp)
  (setq
        a (nth 0 p_list) b (nth 1 p_list)
        a (- a) b (- b)
        a (rtos a 2 1) b (rtos b 2 1)
        tmp (strcat a "B" b)
  )
)

(defun str_list(str ID / len nn pt2 x y p)
  (setq len (strlen str) nn 1)
  (while (< nn len)
     (setq tmp (strcase (substr str nn 1)))
     (if (= tmp ID);if2
       (if (and (= 1 nn) (= ID "B"))  ;input b2 to replace 2b2
         (setq  y (substr str (1+ nn))  x y nn  len)
         (setq x (substr str 1 (1- nn)) y (substr str (1+ nn))  nn  len)
       );if end
     )
     (setq nn (1+ nn))
  );while end
  (if (= nn len) (setq x str y str)) ;if find ID , nn will equal len+1
  (setq x (atof x) y (atof y)
        p (list x y)
  )
)

(defun rec_js(str / n nn tmp)   ;it is ofset value
  (setq len (strlen str) nn 1)
  (while (< nn len)
     (setq tmp (strcase (substr str nn 1)))
     (if (= tmp "B") (setq  nn len tmp "B") (setq nn (1+ nn)))
  )
)

(defun new_pt(p1  p2 / pt1)
  (setq  pt1 (list (+ (car p1) (car p2)) (+ (cadr p1) (cadr p2))))
)

(defun mid_np(pt ap / pt1  pt2 x y ax ay)
  (setq x  (nth 0 pt) y (nth 1 pt)
        ax (nth 0 ap) ay (nth 1 ap)
        ax (/ ax 2)    ay (/ ay 2)
        pt1 (list (+ x ax) (+ y ay))
        pt2 (list (- x ax) (- y ay))
  )
  (setq pt (list pt1 pt2))
)

(defun C:REC(/ pt1 pt2 ap1 ap2 str1  olderr) ;draw a rectuanger
  (setvar "CMDECHO" 0)
  (princ "\nCopyright Y.F.Bao 1996.画矩形,打rec开始本命令.")
  (setq olderr  *error*             ; Initialize variables
        *error* clmerr)
  (setvar "blipmode" 0)
  (setq str1 "0B0" ap2 '(0 0) MD 0)
; (princ "\n输入格式如:1 1B1 1,2 点一点.")
   (initget 129)
   (setq pt1 (getpoint "\n中心点(M)\\相对偏移(格式:xBy)\\矩形的第一角点:"))
   (if (not (listp pt1))(progn
     (if (= "M" (strcase pt1))
       (progn
         (initget 1)
         (setq pt1 (getpoint "\n输入矩形中心点:") MD 1)
       )
       (progn
         (setq ap1 (str_list pt1 "B")
               str1 (list_str ap1)
               ap2  (str_list str1 "B")
         )
         (initget 1)
         (setq pt1 (getpoint "\n输入矩形第一角点的相对点:"))
         (setq pt1 (new_pt ap1 pt1))
       );progn
     );if end
  ));if end
 ;(princ "\n输入格式如: 1 1A2  1B2 点一点 @1,2 1,2 回车.")
 ;(princ (strcat "\n宽(仅宽高相等时)\\宽A高\\第二角点\\相对偏移<" str1 ">:"))
 (if (= MD 1)
   (progn
     (setq pt2 (getstring "\n宽\\宽A高:"))
     (while (= "" pt2) (setq pt2 (getstring "\n宽\\宽A高:")))
     (setq ap2 (str_list pt2 "A")
           pt  (mid_np pt1 ap2)
           pt1 (nth 0 pt) pt2 (nth 1 pt)
     );setq end
   );progn end
   (progn
     (princ (strcat "\n宽\\宽A高\\第二角点\\相对偏移<" str1 ">:"))
     (initget 128)
     (setq pt2 (getcorner pt1))
     (if (null  pt2)
       (progn
         (princ "矩形的第二角点的偏移值:")
         (princ str1)
         (initget 1)
         (setq pt2 (getcorner pt1 "\n矩形的第二角点:"))
         (setq pt2 (new_pt ap2 pt2))
       );progn end
       (progn
         (if (not (listp pt2)) ;if1
           (if (= (rec_js pt2) "B") ;if2
             (progn
               (setq ap2 (str_list pt2 "B"))
               (initget 1)
               (setq pt2 (getcorner pt1 "\n矩形的第二角点:"))
               (setq  pt2 (new_pt ap2 pt2))
             );progn end
             (setq ap2 (str_list pt2 "A")
                   pt2 (new_pt ap2 pt1)
             );setq end
           );if2 end
         );if1 end
       );progn end
     );if end
   );progn end
  );if end
  (command "PLINE" pt1 "w" "0" "" ".x"pt2 ".y"pt1  pt2  ".x"pt1 ".y"pt2  "c")
  (setq *error* olderr)
  (setvar "CMDECHO" 1)
  (princ)
);defun end

