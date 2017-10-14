 (VMON)
(setvar "osmode" 0)
; defun 
(defun judge_xy(x_list y_list) ; / n sum_x sum_y index delta_x delta_y temp)
   (setq n (1- (length x_list))
         sum_x 0.0 sum_y 0.0
         index 0
   ) ;setq end
   (repeat n
        (setq sum_x (+ sum_x (nth index x_list))
              sum_y (+ sum_y (nth index y_list))
              index (1+ index)
        ) ;setq end
   ) ;repeat end
   (setq sum_x (/ sum_x n)
         sum_y (/ sum_y n)
         delta_x 0 delta_y 0
         index 0
   ) ;setq end
   (repeat n
       (setq tempx (- sum_x (nth index x_list))
             tempx (* tempx tempx)
             tempy (- sum_y (nth index y_list))
             tempy (* tempy tempy)
             index (1+ index)
             delta_x (+ delta_x tempx)
             delta_y (+ delta_y tempy)
       ) ;setq end
   ) ;repeat end
   (if (> delta_x delta_y)
       (setq temp "T")
       (setq temp "nil")
   ) ;if end
) ;defun end
;
(defun draw_dash(p_dash)
 (setq p_p1 (polar p_dash pi09 (* 2 scale)) 
       delta (/ bh 3.0)
       p_p2 (polar p_dash pi27 delta)
       p_p3 (polar p_p2 pi27 (/ delta 2.0))
       p_p3 (polar p_p3 pi18 (* 2.5 scale))
       p_p5 (polar p_p2 pi27 delta)
       p_p4 (polar p_p5 pi09 (/ delta 2.0))
       p_p4 (polar p_p4 0 (* 2.5 scale))
       p_p6 (polar p_p5 pi27 (+ delta (* 2 scale)))
 ) ;setq end
 (command "line" p_p1 p_p2 p_p3 p_p4 p_p5 p_p6 "")
) ;defun end
;
; FUNCTION to draw reinforcement dots
(defun draw_dot(xx11 yy11 xx12 yy12 xx21 yy21 xx22 yy22 n1 n2 scale / p1 p2 r1 r2)
        (setq p1 (list xx11 yy11)
              p2 (list xx21 yy21)
              r1 0
              r2 (* 1.0 scale)
        ) ;setq end
        (if (> n1 1)
         (progn
         (command "donut" r1 r2 p1 "")
         (setq delta1 (/ (- yy12 yy11) (- n1 1)))
         (command "array" "l" "" "r" n1 1 delta1)
        ))
        (if (> n2 1)
         (progn
         (command "donut" r1 r2 p2 "")
         (setq delta2 (/ (- yy22 yy21) (- n2 1)))
         (command "array" "l" "" "r" n2 1 delta2)
        ))
) ;defun draw_dot end
;
;FUNCTION to jugde "n1+n2"
(defun if_n12(txt)
  (setq judge 1)
  (if (< (strlen txt) 3) 
        (setq judge 0)
        (progn
         (setq l1 (substr txt 1 1)
               l2 (substr txt 2 1)
               l3 (substr txt 3 1)
         ) ;setq end
         (if (or (< l1 "0") (> l1 "9")) (setq judge 0))
         (if (or (< l3 "0") (> l3 "9")) (setq judge 0))
         (if (/= l2 "+") (setq judge 0))
        ) ;progn end
  ) ;if end
  (if (= judge 0)
      (setq y "T")   
      (setq y "nil")
   ) ;if end
) ;defun if_n12 end
; 
(defun C:ZLK( )
 (setvar "CMDECHO" 0)
 (setvar "blipmode" 0)
 (command "layer" "m" "rei" "c" "1" "" "s" "0" "")
 (command "layer" "m" "pdim" "c" "green" "" "s" "0" "")
 ;(command "layer" "s" "0" "")
 (setq H (getreal "\n柱宽(mm):"))
 (setq B (getreal "\n柱高(mm):"))
 (setq scale (getvar "USERR1"))
 (setq bh 0)
 (setq bl (if (= bh 0) 0 (* 10 scale)))
 (setq oer *error* *error* clmerr)
 (princ "\n局部比例:<")
 (princ scale)
 (setq txt (getstring ">"))
 (if (/= (ascii txt) 0)
     (setq scale1 (atoi txt))
     (setq scale1 scale)
 ) ;if end
 (setq diml (getvar "DIMLFAC"))
 (setvar "DIMLFAC" (/ scale1 scale))
 (setq factor (/ scale scale1))
 (setq dx (* b factor)
       dy (* h factor)
       bh (* bh factor)
       insl (+ dx (* 75 scale)) 
 ) ;setq end
 (command "zoom" "a")
 (princ "\n插入点:")
 (command "insert" "border" "x" insl "y" insl "r" 0 pause)
 (setq p1 (getvar "lastpoint")
       zp1 p1
       zp2 (polar zp1 0 insl)
       zp2 (polar zp2 (* 0.5 pi) insl)
       p1  (polar p1 0 (* 30 scale))
       p1  (polar p1 (* 0.5 pi) (* 30 scale))
 ) ; setq end
 (command "erase" "l" "")
 (command "zoom" "w" zp1 zp2)
 (setq x1 (nth 0 p1)
       y1 (nth 1 p1)
       x2 (+ x1 dx)
       y2 y1
       x3 x1
       y3 (+ y1 dy)
       x4 x2
       y4 y3
 ) ;setq end
 (setq dxy (* 1.5 scale))
 (setq x11 (+ x1 dxy)
       y11 (+ y1 dxy)
       x21 (- x2 dxy)
       y21 (+ y2 dxy)
       x31 (+ x3 dxy)
       y31 (- y3 dxy)
       x41 (- x4 dxy)
       y41 (- y4 dxy)
       ) ;setq end
 (setq p1  (list x1 y1)
       p2  (list x2 y2)
       p3  (list x3 y3)
       p4  (list x4 y4)
       p11 (list x11 y11)
       p21 (list x21 y21)
       p31 (list x31 y31)
       p41 (list x41 y41)
       ) ; setq end
 (setq p5 (polar p3 pi18 bl)
       p6 (polar p5 pi27 bh)
       p7 (polar p3 pi27 bh)
       p8 (polar p4 0 bl)
       p9 (polar p8 pi27 bh)
       p10 (polar p4 pi27 bh)
 ) ;setq end
 (command "line" p6 p7 p1 p2 p10 p9 "")
 (command "line" p5 p8 "")
 (command "layer" "s" "rei" "")
 (command "line" p11 p21 p41 p31 "c")
 (command "layer" "s" "0" "")
 (setvar "CMDECHO" 1)
);DEFUN LLK() END 
;
;
(DEFUN C:ZJBZ()
 (setvar "CMDECHO" 0)
 (setq oer *error* *error* clmerr)
 (command "layer" "s" "0" "")
 (princ "\n平面内钢筋,最多可布置一排:")
 (setq n1 (getint "\n左边钢筋<n1>:"))
 (setq dxy1 (* 1.0 scale)
       dxy2 (* 3.0 scale)
       n2 0
       n3 0
 ) ;setq end
 (setq x12 (+ x11 dxy1)
       y12 (+ y11 dxy1)
       x32 (+ x31 dxy1)
       y32 (- y31 dxy1)
       x13 (+ x12 dxy2)
       y13 y12
       x33 (+ x32 dxy2)
       y33 y32 
 ) ;setq end
 (draw_dot x12 y12 x32 y32 x13 y13 x33 y33 n1 1 scale)
;
 (princ (strcat "\n右边钢筋<" (itoa n1) ">:" ))
 (setq n4 (if (setq t_emp (getint)) t_emp n1))
 (setq dxy1 (* 1.0 scale)
       dxy2 (* 3.0 scale))
 (setq x22 (- x21 dxy1)
       y22 (+ y21 dxy1)
       x42 (- x41 dxy1)
       y42 (- y41 dxy1)
       x23 (- x22 dxy2)
       y23 y22
       x43 x23
       y43 y42
       ) ;setq end
 (draw_dot x23 y23 x43 y43 x22 y22 x42 y42 1 n4 scale)
 (princ "\n平面外钢筋,不含两端钢筋,最多一排:")
 (setq n5 (getint "\n上部钢筋:")
       p32 (list x32 y32)
       p33 (list x33 y33)
       p42 (list x42 y42)
       p43 (list x43 y43)
       p_1 (if (> n2 1) p33 p32)
       p_2 (if (> n3 1) p43 p42)
       delta (/ (distance p_1 p_2) (+ n5 1))
       p_1 (polar p_1 0 delta)
       r1 0 r2 scale
 ) ;setq end
 (command "layer" "s" "0" "")
 (repeat n5
   (command "donut" r1 r2 p_1 "")
   (Setq p_1 (polar p_1 0 delta))
 ) ;repeat end
 (princ (strcat "\n下部钢筋:<" (itoa n5) ">"))
 (setq n6 (if (setq t_emp (getint)) t_emp n5)
       p12 (list x12 y12)
       p13 (list x13 y13)
       p22 (list x22 y22)
       p23 (list x23 y23)
       p_1 (if (> n2 1) p13 p12)
       p_2 (if (> n3 1) p23 p22)
       delta (/ (distance p_1 p_2) (+ n6 1))
       p_1 (polar p_1 0 delta)
 ) ;setq end
 (repeat n6
   (command "donut" r1 r2 p_1 "")
   (Setq p_1 (polar p_1 0 delta))
 ) ;repeat end
) ;DEFUN zJBZ() END
;
; b dim
(DEFUN C:ZCC() 
 (setvar "CMDECHO" 0)
 (setvar "DIMLFAC" (/ scale1 scale))
 (setq oer *error* *error* clmerr)
 (command "layer" "s" "pdim" "")
 (setq pp (getpoint "\n高度尺寸线位置:"))
 (setq xm (nth 0 pp)
       ym (nth 1 pp)
       dy  (* 10 scale)
       xl x1
       yl (+ ym dy)
       xr x2
       yr yl
       pl (list xl yl)
       pr (list xr yr)
 ) ;setq end
 (command "dim1" "hor" pl pr pp "")
; h dim 
 (setq pp (getpoint "\n宽度尺寸线位置:"))
 (setq xm (nth 0 pp)
       ym (nth 1 pp)
       dx (* 10 scale)
       xl (- xm dx)
       yl y2
       xr xl
       yr y4
       pl (list xl yl)
       pr (list xr yr)
 ) ;setq end
 (command "dim1" "ver" pl pr pp "")
 (command "zoom" "p")
 (setvar "DIMLFAC" diml)
 (setvar "CMDECHO" 1)
 (command "layer" "s" "pdim" "")
) ;defun LCC() end
;
; to number the reinforcement
;
(DEFUN C:ZJBH()
 (setvar "CMDECHO" 0)
 (setq oer *error* *error* clmerr)
 (command "layer" "s" "0" "")
 (setq txt (getstring "\n钢筋号:"))
 (while (/= txt "")     
        (setq index 0
              xlist '(0.0)
              ylist '(0.0)
        ) ;setq end
        (setq xmax x1
              xmin x2
              ymax y1
              ymin y3
        )
        (princ "\n用光标点取") 
        (princ txt) 
        (princ "号钢筋:")
        (setq rp (getpoint "\n钢筋点:"))
        (princ "\nok!")
        (while rp
          (setq x (nth 0 rp)
                y (nth 1 rp)
                xlist (cons x xlist)
                ylist (cons y ylist)
                index (+ 1 index)
           ) ;setq end
           (if (> x xmax) (setq xmax x))
           (if (< x xmin) (setq xmin x))
           (if (> y ymax) (setq ymax y))
           (if (< y ymin) (setq ymin y))
           (setq rp (getpoint "\n钢筋点:<按回车键结束>"))
        ) ;while1 end
        (setq pp (getpoint "\n用光标确定此编号位置:"))
        (setq ppx (nth 0 pp)
              ppy (nth 1 pp)
              xmid (/ (+ xmin xmax) 2.0)
              ymid (/ (+ ymin ymax) 2.0)
              dx (* 20 scale) dy dx
              rei (getstring "\n钢筋规格,如 5%%13220 >:")
        )
       (setq fac_xy (judge_xy  xlist ylist))
      (if (= fac_xy "T")  
       (progn 
        (if (<= ppx xmid)
            (setq xl (- x1 dx)
                  yl ppy 
                  xr xmax
                  yr ppy
                  xnum (- xl (* 3.0 scale))
                  ynum ppy
                  x_text (+ xl (* 2.0 scale))
                  y_text (+ ppy (* 1.0 scale))
             ) ;setq end
             (setq xl xmin
                   yl ppy
                   xr (+ x2 dx)
                   yr ppy
                   xnum (+ xr (* 3.0 scale))
                   ynum ppy
                   x_text (+ x2 (* 2.0 scale))
                   y_text (+ ppy (* 1.0 scale))
              ) ;setq end
        ) ;if end
        (setq ang_txt 0)
       ) ;progn end 
       (progn 
        (if (<= ppy ymid)
            (setq yl (- y1 dy)
                  xl ppx 
                  yr ymax
                  xr ppx
                  ynum (- yl (* 3.0 scale))
                  xnum ppx
                  y_text (+ yl (* 2.0 scale))
                  x_text (- ppx (* 1.0 scale))
             ) ;setq end
             (setq yl ymin
                   xl ppx
                   yr (+ y3 dy)
                   xr ppx
                   ynum (+ yr (* 3.0 scale))
                   xnum ppx
                   y_text (+ y3 (* 2.0 scale))
                   x_text (- ppx (* 1.0 scale))
              ) ;setq end
        ) ;if end
        (setq ang_txt 90)
       ) ;progn end
      ) ;if end
        (setq pl (list xl yl)
              pr (list xr yr)
              pnum (list xnum ynum)
              p_text (list x_text y_text)
        ) ;setq end
        (command "line" pl pr "")
        (setq htxt (* 3.0 scale))
        (command "circle" pnum htxt)
        (command "text" "j" "m" pnum htxt ang_txt txt)
        (command "text" p_text htxt ang_txt rei)
        (setq index1 0)
       (repeat index
        (if (= fac_xy "T") 
         (setq xb (nth index1 xlist)
               yb (nth index1 ylist)
               xe xb
               ye ppy
               pb (list xb yb)
               pe (list xe ye)
               index1 (+ 1 index1)
         ) ;setq end
         (setq xb (nth index1 xlist)
               yb (nth index1 ylist)
               ye yb
               xe ppx
               pb (list xb yb)
               pe (list xe ye)
               index1 (+ 1 index1)
         ) ;setq end
        ) ;if end
        (command "line" pb pe "")
       ) ;repeat end
        (setq txt (getstring "\n钢筋号:<按回车键结束编号>"))
 ) ;while2 end
 (setvar "CMDECHO" 1)
) ;DEFUN LJBH END
;
(defun C:ZLXG( / p1 temp)
  (setq temp "T")
  (setvar "pdmode" 0)
  (command "layer" "s" "rei" "")
  (while (setq p1 (getpoint "\n第一点:"))
         (princ "\n第二点:")
         (command "line" p1 pause "")
  ) ;while end
  (command "layer" "s" "0" "")
) ;defun end
