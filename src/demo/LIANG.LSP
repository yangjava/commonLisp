;+-------------------+
;+文件名：LIANG.lsp  +
;+-------------------+
; 画折断线函数
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
; 画钢筋点函数
(defun draw_dot(xx11 yy11 xx12 yy12 xx21 yy21 xx22 yy22 n1 n2 scale)
        (setq p1 (list xx11 yy11)
              p2 (list xx21 yy21)
              r1 0
              r2 (* 1.0 scale)
        ) ;setq end
        (if (> n1 1)
         (progn
          (command "donut" r1 r2 p1 "")
          (setq delta1 (/ (- xx12 xx11) (- n1 1)))
          (command "array" "l" "" "r" 1 n1 delta1)
         ) ;progn end
        )  ;if end
        (if (> n2 1)
         (progn
          (command "donut" r1 r2 p2 "")
          (setq delta2 (/ (- xx22 xx21) (- n2 1)))
          (command "array" "l" "" "r" 1 n2 delta2)
         ) ;progn end
        ) ;if end
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
(defun C:LLK( )
 (inivar) ;初始化系统变量
 (command "layer" "m" "rei" "c" "1" "" "s" "0" "")
 (command "layer" "m" "pdim" "c" "green" "" "s" "0" "")
 (command "layer" "s" "0" "")
 (setq b (getreal "\n截面宽(mm):"))
 (setq h (getreal "\n截面高(mm):"))
 (setq bh (if 
           (setq temp (getreal "\n板厚(mm)<0>:"))
           temp 0) ;if end
 ) ;setq end
 (setq scale (getvar "USERR1"))
 (setq bl (if (= bh 0) 0 (* 10 scale)))
 (princ "\n局部比例:<")
 (princ scale)
 (setq txt (getstring ">"))
 (if (/= (ascii txt) 0)
     (setq scale1 (atoi txt))
     (setq scale1 scale)
 ) ;if end
 (setq diml (getvar "DIMLFAC"))
 (setvar "DIMLFAC" (/ scale1 scale))
 (setq factor (/ scale scale1) ;计算放大比例
       dx (* b factor)
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
 (command "line" p6 p7 p1 p2 p10 p9 "") ;画轮廓线
 (command "line" p5 p8 "")
 (command "layer" "s" "rei" "")
 (command "line" p11 p21 p41 p31 "c")
 (command "layer" "s" "0" "")
 (if (> bh 0.0) ;如果板厚>0
  (progn 
    (draw_dash p5) ;画折断线
    (draw_dash p8)
  ) ;progn end
 ) ;if end
 (resvar) ;还原系统变量
 (princ)
);DEFUN LLK() END 
;
;梁钢筋逗点标注
(DEFUN C:LJBZ()
 (inivar) ;初始化系统变量
 (command "layer" "s" "0" "")
 (setq txt (getstring "\n上部钢筋<n1+n2>,如5+3:"))
 (setq temp (if_n12 txt))
 (while (/= temp "nil")
        (setq txt (getstring "\n输入错误,重新输入:"))
        (setq temp (if_n12 txt))
 ) ;while end
 (setq n1 (atoi (substr txt 1 1)))
 (setq n2 (atoi (substr txt 3 1)))
 (setq dxy1 (* 1.0 scale)
       dxy2 (* 3.0 scale)
 ) ;setq end
 (setq x32 (+ x31 dxy1)
       y32 (- y31 dxy1)
       x42 (- x41 dxy1)
       y42 (- y41 dxy1)
       x33 x32
       y33 (- y32 dxy2)
       x43 x42
       y43 (- y42 dxy2)
       ) ;setq end
 (draw_dot x32 y32 x42 y42 x33 y33 x43 y43 n1 n2 scale)
;
 (setq txt (strcat (itoa n2) "+" (itoa n1))
       txt1 (strcat "\n下部钢筋(n3+n4):<" txt ">")
 ) ;setq end
 (princ txt1)
 (setq t_emp (getstring)
       txt (if (= t_emp "") txt t_emp)
       temp (if_n12 txt)
 ) ;setq end
 (while (/= temp "nil")
        (setq txt (getstring "\n输入错误,重新输入:"))
        (setq temp (if_n12 txt))
 ) ;while end
 (setq n3 (atoi (substr txt 1 1))
       n4 (atoi (substr txt 3 1))
       dxy1 (* 1.0 scale)
       dxy2 (* 4.0 scale)
       x12 (+ x11 dxy1)
       y12 (+ y11 dxy2)
       x22 (- x21 dxy1)
       y22 y12
       x13 x12
       y13 (+ y11 dxy1)
       x23 x22
       y23 y13
 ) ;setq end
 (draw_dot x12 y12 x22 y22 x13 y13 x23 y23 n3 n4 scale) 
 (resvar) ;还原系统变量
 (princ)
) ;DEFUN LJBZ() END
;
;标注尺寸
(DEFUN C:LCC() 
 (inivar) ;初始化系统变量
 (setvar "DIMLFAC" (/ scale1 scale))
 (command "layer" "s" "pdim" "")
 (setq pp (getpoint "\n宽度尺寸线位置:")
       xm (nth 0 pp)
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
 (setq pp (getpoint "\n高度尺寸线位置:")
       xm (nth 0 pp)
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
 (command "layer" "s" "pdim" "")
 (resvar) ;还原系统变量
 (princ)
) ;defun LCC() end
;
;钢筋编号
(DEFUN C:LJBH()
 (inivar) ;初始化系统变量
 (command "layer" "s" "0" "")
 (setq txt (getstring "\n钢筋号:"))
 (while (/= txt "")     
        (setq index 0
              xlist '(0.0)
              ylist '(0.0)
              xmax x1
              xmin x2
        ) ;setq end
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
           (setq rp (getpoint "\n钢筋点:<按回车键结束>"))
        ) ;while1 end
        (setq pp (getpoint "\n用光标确定此编号位置:")
              ppx (nth 0 pp)
              ppy (nth 1 pp)
              xmid (/ (+ xmin xmax) 2)
              dx (* 20 scale)
        ) ;setq end
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
        (setq pl (list xl yl)
              pr (list xr yr)
              pnum (list xnum ynum)
              p_text (list x_text y_text)
              rei (getstring "\n钢筋规格,如 5%%13220 >:")
        ) ;setq end
        (command "line" pl pr "")
        (setq htxt (* 3.0 scale))
        (command "circle" pnum htxt)
        (command "text" "j" "m" pnum htxt 0 txt)
        (command "text" p_text htxt 0 rei)
        (setq index1 0)
        (repeat index
         (setq xb (nth index1 xlist)
               yb (nth index1 ylist)
               xe xb
               ye ppy
               pb (list xb yb)
               pe (list xe ye)
               index1 (+ 1 index1)
         ) ;setq end
         (command "line" pb pe "")
        ) ;repeat end
        (setq txt (getstring "\n钢筋号:<按回车键结束编号>"))
 ) ;while2 end
 (resvar) ;还原系统变量
 (princ)
) ;DEFUN LJBH END

;画梁腰筋
(DEFUN C:LYJ()
 (inivar) ;初始化系统变量
 (setvar "orthomode" 1)
 (command "layer" "s" "0" "")
 (setq scale (getvar "userr1")) 
 (while (setq py1 (getpoint "\n腰筋第一点:<按回车键结束>"))
  (princ "\n第二点:") 
  (command "line" py1 pause "")
  (setq py2 (getvar "lastpoint"))
  (command "erase" "l" "")
  (setq xx1 (nth 0 py1) 
        yy1 (nth 1 py1)
        xx2 (nth 0 py2) 
        yy2 (nth 1 py2)
        d (- (+ xx2 yy2) (+ xx1 yy1))
  ) ;setq end
  (if (< d 0.0) 
      (setq temp py1 
            py1 py2 
            py2 temp
      ) ;setq end
  ) ;if end
  (setq R1 0 r2 (* 1.0 scale))
  (command "donut" r1 r2 py1 py2 "")
  (setq py1 (polar py1 pi27 (* 1.0 scale))
        py2 (poLar py2 pi09 (* 1.0 scale))
        py3 (poLar py1 pi09 (* 2.0 scale))
        py4 (polar py3 (* 0.17 pi) (* 2.0 scale))
        py5 (polar py2 pi27 (* 2.0 scale))
        py6 (polar py5 (* 1.17 pi) (* 2.0 scale))
  )
  (command "layer" "s" "rei" "")
  (command "line" py1 py2 "")
  (command "line" py3 py4 "")
  (command "line" py5 py6 "")
  (command "layer" "s" "0" "")
 ) ;while end 
 (resvar) ;还原系统变量
 (princ)
) ;defun end
