(command "layer" "m" "rei" "c" "1" "" "s" "0" "")
(command "layer" "m" "pdim" "c" "green" "" "s" "0" "")
(command "layer" "m" "axis" "c" "1" "" "s" "0" "")
;画折断线
(defun llm_dash(p_dash)
 (setq p_p1 (polar p_dash pi18 (+ (/ b 2) (* 2 scale)))
       p_p1 (polar p_p1 pi27 (* 10 scale))
       delta (/ b 3.0)
       p_p2 (polar p_p1 0 (+ (* 2 scale) delta))
       p_p3 (polar p_p2 0 (/ delta 2.0))
       p_p3 (polar p_p3 (* pi09 0.5) (* 3 scale))
       p_p5 (polar p_p2 0 delta)
       p_p4 (polar p_p5 pi18 (/ delta 2.0))
       p_p4 (polar p_p4 (* pi09 -1.5) (* 3 scale))
       p_p6 (polar p_p5 0 (+ delta (* 2 scale)))
 ) ;setq end
 (command "line" p_p1 p_p2 p_p3 p_p4 p_p5 p_p6 "")
) ;defun end
;
;----------
;总信息输入
;----------
(DEfun C:SZXX( / x y txt l-zxx p0 p1 x0 y dy ny rea htext)
 (inivar) ;初始化系统变量
 (setq scale (getvar "userr1")
       b 600   ;设缺省值
       blh 100
       j-wz 0
       j-cc 0
       j-gj1 2
       j-gj2 2
       l-zxx '("\n支座宽:" "\n梁高局部比例:" "\n梁是否在顶层(1/0):"
               "\n是否注实际尺寸(1/0):" "\n几排支座负筋:" "\n几排跨中钢筋:")
 ) ;setq end
 (setq j-zc (getstring "\n主/次梁(1/0):<1>")
       j-zc (if (= j-zc "") "1" j-zc)
 ) ;setq end
 (command "zoom" "a")
 (princ "\n输总信息插入点:")
 (command "insert" "t_zxx" "x" scale "y" scale "r" 0 pause)
 (setq p0 (getvar "lastpoint"))
 (setq x (* 60 scale)
       y (* 70 scale)
       z1 (polar p0 0 x)
       z1 (polar z1 pi09 y)
 ) ;setq end
 (command "zoom" "w" p0 z1)
 (while (setq p1 (getpoint "\n用光标点取修改:<回车键结束修改>"))
  (setq y (nth 1 p1)  ;根据光标点取的位置计算行数
        y0 (nth 1 p0)
        dy (- y y0)
        ny (fix (/ dy (* 10.0 scale)))
        ny (- 6 ny)
        txt (nth (1- ny) l-zxx) ;获取提示信息
  ) ;setq end
  (princ txt)
  (setq txt (getstring) ;等待用户输入
        htext (* 4.0 scale)
        rea (atoi txt) ;转换成整数
  ) ;setq end
  (command "text" p1 htext 0 txt)
  (cond  
    ((= ny 1) (setq b rea))
    ((= ny 2) (setq blh rea))
    ((= ny 3) (setq j-wz rea))
    ((= ny 4) (setq j-cc rea))
    ((= ny 5) (setq j-gj1 rea))
    ((= ny 6) (setq j-gj2 rea))
  ) ;cond end
  (if (and (= ny 4) (= txt "1"))
    (setq textd (getstring "\n负筋断点离支座距离(mm):")
          textc (if (= j-zc "1") (getstring "\n箍筋加密区长度(mm):") "0")
    ) ;setq end      
  )   ;if end
 )  ;while end
 (if (= j-zc "0") (setq j-wz 1))
 (command "erase" "c" p0 z1 "")
 (command "zoom" "p")
 (resvar) ;还原系统变量
 (princ)
) ;defun end
;
;------------
;输入各跨跨度、梁高信息
;------------
(defun C:GKBZ( / temp)
 (inivar) ;初始化系统变量
 (setq temp T)
 (while temp
  (princ "\n输入各跨跨度,如为挑梁,跨度为负,如 -4,3(8),7\n>(米):")
  (load "tab.lsp")
  (setq scale 1000)
  (setq l-l (readlh))
  (princ "\n输入各跨梁高,如 .7,4(.6)\n>(米):")
  (setq l-h (readlh)    
        n-l (length l-l)
        n-h (length l-h)
  ) ;setq end
  (setq scale (getvar "userr1"))
  (if (/= n-l n-h) (princ "\n输入错误:梁高个数与跨数不符"))
  (setq m 0 index T)
  (while index
   (setq at (nth m l-l)
         t1 (/= m 0)
         t2 (/= m (1- n-l))
         t3 (< at 0.0)
         t4 (and t1 t2)
    ) ;setq end
   (if (and t3 t4 (< m  n-l))     
    (progn (princ "\n输入错误:挑梁数据错") (princ "\naaaaaaa"))
    (progn (setq temp nil) (princ temp) (setq index nil))
   ) ;if end
   (setq m (1+ m))
  ) ;while1 end
 ) ;while end
 (resvar) ;还原系统变量
 (princ)
) ;defun end  
;
;画单跨梁立面函数
(defun draw_beam(l h p0)
 ;画轴线
 (command "layer" "s" "0" "")
 (setq pax (polar p0 pi27 (* 65 scale))
       pof (polar p0 0 (* 10 scale))
 ) ;setq end
 (princ "\n line1")
 (command "line" p0 pax "")
 (command "change" "l" "" "p" "la" "axis" "")
 (command "array" "l"  "" "r" 1 2 l)
 ;画下边轮廓线
 (setq scale (getvar "userr1")
       hh (* h (/ scale blh))
       b2 (/ b 2)
       d_p (if (= j-zc "1") (* 10 scale) (* 5 scale))
       p2 (polar p0 0 b2)
       p1 (polar p2 pi27 d_p)
       p3 (polar p2 0 (- l b))
       p4 (polar p3 pi27 d_p)
       p1_1 (polar p1 pi18 b)
       p4_1 (polar p4 0    b)
 ) ;setq end
 (if (= j-zc "1")
  (command "line" p1 p2 p3 p4 "")
  (command "line" p1_1 p1 p2 p3 p4 p4_1 "")
 ) ;if end
 ;画上边轮廓线
 (setq s2 (polar p2 pi09 hh)
       s1 (if (= j-wz 1) (polar s2 pi18 b2) (polar s2 pi09 (* 10.0 scale)))
       s3 (polar s2 0 (- l b))
       s4 (if (= j-wz 1) (polar s3 pi0 b2) (polar s3 pi09 (* 10.0 scale)))
 ) ;setq end      
 (command "line" s1 s2 s3 s4 "")
 ;画下部钢筋线
 (setq pr1 (polar p0 pi09 (* 1.0 scale))
       pr2 (polar pr1 0 l)
       d (if (= j-gj2 2) (* 1 scale) 0)
       pr3 (polar pr1 pi09 d)
       pr4 (polar pr2 pi09 d)
 ) ;setq end
 (command "layer" "s" "rei" "")
 (command "line" pr1 pr2 "")
 (command "line" pr3 pr4 "")
 ;画上部钢筋线
 (setq pr1 (polar p0 pi09 (- hh (* 1.0 scale)))
       pr2 (polar pr1 0 l)
       d (if (= j-gj1 2) (* 1 scale) 0)
       pr3 (polar pr1 pi27 d)
       pr4 (polar pr2 pi27 d)
       pr31 (polar pr3 0 (+ b2 (/ l 3.0)))
       pr41 (polar pr4 pi18 (+ b2 (/ l 3.0)))
       pr32 (polar pr31 (* -0.75 pi) (* 2. scale))
       pr42 (polar pr41 (* -0.25 pi) (* 2. scale))
 ) ;setq end
 (command "line" pr1 pr2 "")
 (command "line" pr3 pr31 pr32 "")
 (command "line" pr4 pr41 pr42 "")
 ;画箍筋
 (setq pg1 (polar pr1 0 (+ b2 (* 1.0 scale)))
       pg2 (polar pg1 pi27 (- hh (* 2.0 scale)))
       d (- (* 1.5 h) (* 1.0 scale))
 ) ;setq end
 (command "line" pg1 pg2 "")
 (if (= j-zc "1") (command "array" "l" "" "r" 1 2 d) )
 (setq pg3 (polar pr2 pi18 (+ b2 (* 1.0 scale)))
       pg4 (polar pg3 pi27 (- hh (* 2.0 scale)))
       d (+ (* -1.5 h) (* 1.0 scale))
 ) ;setq end
 (command "line" pg3 pg4 "")
 (if (= j-zc "1") (command "array" "l" "" "r" 1 2 d) )
 ;画尺寸线
 (setq textb (if (= j-cc 0) "b" (rtos (/ b 2.0)))
       textl (if (= j-cc 0) "L" (rtos l))
       textd (if (= j-cc 0) "d" textd)
       textc (if (= j-cc 0) "c" textc)
 ) ;setq end
 (setq dbl1 (polar p0 pi27 (* 30 scale))
       dbl2 (polar dbl1 0 b2)
       dbl3 (polar dbl2 0 (* 1.5 h))
       dbr1 (polar dbl1 0 l)
       dbr2 (polar dbr1 pi18 b2)
       dbr3 (polar dbr2 pi18 (* 1.5 h))
       dp (polar dbl1 pi27 (* 15 scale))       
 ) ;setq end      
 (command "layer" "s" "pdim" "")
 (setvar "dimdli" 0)
 (if (= j-zc "1")
  (progn
   (command "dim" "hor" dbl1 dbl2 dp textb "" dbl2 dbl3 dp textc "e")
   (command "dim" "hor" dbr1 dbr2 dp textb "" dbr2 dbr3 dp textc "e")
  ) ;progn end
  (progn
   (command "dim" "hor" dbl1 dbl2 dp textb "e")
   (command "dim" "hor" dbr1 dbr2 dp textb "e")
  ) ;progn end
 ) ;if end
 (setq dll dp
       dlr (polar dll 0 l)
       dp (polar dll pi27 (* 15 scale))
 ) ;setq end
 (command "dim" "hor" dll dlr dp textl "e")
 (setq ddl1 (polar p0 pi09 (+ hh (* 20 scale)))
       ddl1 (polar ddl1 0 b2)
       ddl2 (polar ddl1 0 (/ l 3.0))
       dp   (polar ddl1 pi09 (* 10 scale))
 ) ;setqend
 (command "dim" "hor" ddl1 ddl2 dp textd "e")
 (setq ddl1 (polar p0 pi09 (+ hh (* 20 scale)))
       ddl1 (polar ddl1 0 l)
       ddl1 (polar ddl1 pi18 b2)
       ddl2 (polar ddl1 pi18 (/ l 3.0))
       dp   (polar ddl1 pi09 (* 10 scale))
 ) ;setqend
 (command "dim" "hor" ddl1 ddl2 dp textd "e")
 (command "layer" "s" "0" "")
);defun end

;----------
;画挑梁立面
;----------
(defun draw_tl(l h p0 direc)
 ;画轮廓线
 (command "layer" "s" "0" "")
 (setq ang1 (if (= direc 1) pi18 0)
       ang2 pi09
       ang3 (if (= direc 1) 0 pi18)
       ang4 (if (= direc 1) (* -0.25 pi) (* -0.75 pi))
       ang5 (if (= direc 1) (* 0.25 pi) (* 0.75 pi))
       scale (getvar "userr1")
       hh (* h (/ scale blh))
       b2 (/ b 2)
       p2 (polar p0 ang1 b2)
       p1 (polar p2 pi27 
                        (if (= j-zc "1") (* 10 scale) (* 5 scale) )
          ) ;polar end
       p3 (polar p2 ang1 l)
       p4 (polar p3 ang2 hh)
       p5 (polar p4 ang3 l)
       p6 (if (= j-wz 1) (polar p5 ang3 b2) (polar p5 pi09 (* 10. scale)))
 ) ;setq end
 (command "line" p1 p2 p3 p4 p5 p6 "")
 ;画下部钢筋线
 (setq pr1 (polar p0 pi09 (* 1.0 scale))
       pr2 (polar pr1 ang1 (+ l b2 (* -3 scale)))
       pr21 (polar pr2 pi09 (* 0.3 hh))
       pr22 (polar pr21 ang4 (* 2.0 scale))
       d (if (= j-gj2 2) (* 1 scale) 0)
       pr3 (polar pr1 pi09 d)
       pr4 (polar pr2 pi09 d)
 ) ;setq end
 (command "layer" "s" "rei" "")
 (command "line" pr1 pr2 pr21 pr22 "")
 (command "line" pr3 pr4 "")
 ;画上部钢筋线
 (setq pr1 (polar p0 pi09 (- hh (* 1.0 scale)))
       pr2 (polar pr1 ang1 (+ l b2 (* -1 scale)))
       pr21 (polar pr2 pi27 (* 0.6 hh))
       pr22 (polar pr21 ang5 (* 2.0 scale))
       d (if (= j-gj1 2) (* 1 scale) 0)
       pr3 (polar pr1 pi27 d)
       pr4 (polar pr2 pi27 d)
 ) ;setq end
 (command "line" pr1 pr2 pr21 pr22 "")
 (command "line" pr3 pr4 "")
 ;画箍筋
 (setq pg1 (polar pr1 ang1 (+ b2 (* 1.0 scale)))
       pg2 (polar pg1 pi27 (- hh (* 2.0 scale)))
       d (if (= direc 1) (* -1.5 h) (* 1.5 h))
 ) ;setq end
 (command "line" pg1 pg2 "")
 (if (= j-zc "1") (command "array" "l" "" "r" 1 2 d) )
 ;画尺寸线
 (setq textb (if (= j-cc 0) "b" (rtos (/ b 2.0)))
       textl (if (= j-cc 0) "L" (rtos l))
       textd (if (= j-cc 0) "d" textd)
       textc (if (= j-cc 0) "c" textc)
 ) ;setq end
 (setq dbl1 (polar p0 pi27 (* 30 scale))
       dbl2 (polar dbl1 ang1 b2)
       dp (polar dbl1 pi27 (* 15 scale))       
 ) ;setq end      
 (command "layer" "s" "pdim" "")
 (setvar "dimdli" 0)
 (command "dim" "hor" dbl1 dbl2 dp textb "e")
 (setq dll (polar dp ang1 b2)
       dlr (polar dll ang1 l)
       dp (polar dll pi27 (* 15 scale))
 ) ;setq end
 (command "dim" "hor" dll dlr dp textl "e")
 (command "layer" "s" "0" "")
);defun end

;---------
;画端跨支座
;---------
(defun draw_dt(l h p0 direc)
 (setq ang1 (if (= direc 1) pi18 0)
       ang2 pi09
       ang3 (if (= direc 1) 0 pi18)
       ang4 (if (= direc 1) (* -0.25 pi) (* -0.75 pi))
       ang5 (if (= direc 1) (* 0.25 pi) (* 0.75 pi))
       hh (* h (/ scale blh))
       b2 (/ b 2)
       scale (getvar "userr1") 
 )      
 (setq p1 (polar p0 ang1 b2)
       p2 (polar p1 pi09 hh)
       d_p (if (= j-zc "1") (* 10 scale) (* 5 scale) )
       p1 (polar p1 pi27 d_p)
       p3 (if (= j-wz 1) (polar p2 ang3 b2) (polar p2 pi09 (* 10 scale)))
 )
 (command "layer" "s" "0" "")
 (command "line" p1 p2 p3 "")
 ;画下部钢筋线
 (setq pr1 (polar p0 pi09 (* 1.0 scale))
       pr2 (polar pr1 ang1 (+ b2 (* -2 scale)))
       pr21 (polar pr2 pi09 (* 0.3 hh))
       pr22 (polar pr21 ang4 (* 2.0 scale))
       d (if (= j-gj2 2) (* 1 scale) 0)
       pr3 (polar pr1 pi09 d)
       pr4 (polar pr2 pi09 d)
 ) ;setq end
 (command "layer" "s" "rei" "")
 (command "line" pr1 pr2 pr21 pr22 "")
 (command "line" pr3 pr4 "")
 ;画上部钢筋线
 (setq pr1 (polar p0 pi09 (- hh (* 1.0 scale)))
       pr2 (polar pr1 ang1 (+ b2 (* -1 scale)))
       d (if (= j-gj1 2) (* 1 scale) 0)
       pr21 (polar pr2 pi27 (* 1.2 hh))
       pr22 (polar pr21 ang5 (* 2.0 scale))
       pr3 (polar pr1 pi27 d)
       pr4 (polar pr2 pi27 d)
 ) ;setq end
 (command "line" pr1 pr2 pr21 pr22 "")
 (command "line" pr3 pr4 "")
) ;defun end

;------------
;画梁立面
;------------
(defun C:HL()
 (inivar) ;初始化系统变量
 (setq n-len (length l-l)
       add 0 
       scale (getvar "userr1")
       m 0 
 ) ;setq end
 (repeat n-len
   (setq add (+ add (abs (nth m l-l)))
         m (1+ m)
   );setq end
 ) ;repeat end
 (setq y (+ (nth 0 l-h) (* 70 scale)))
 (princ "\n梁插入点:")
 (command "insert" "border" "x" add "y" y "r" 0 pause)
 (setq p0 (getvar "lastpoint"))
 (command "erase" "l" "")
 (setq l (nth 0 l-l) h (nth 0 l-h))
 (if (< l 0.0) ;判断是否挑梁
  (progn
    (setq l (abs l))
    (setq direc 1)
    (draw_tl  l h p0 direc)
    (if (> n-len 1) 
     (setq dh (* (- (nth 1 l-h) h) (/ scale blh))
           p0 (polar p0 pi27 dh)
     );setq end
    ) ;if end
  ) ;progn end
  (progn
    (setq direc 1)
    (draw_dt  l h p0 direc)
    (draw_beam  l h p0)
    (if (= j-zc "1") (llm_dash p0))
    (if (= j-wz 0) 
        (llm_dash (polar p0 pi09 (+ hh (* 20 scale))))
    ) ;if end
    (if (> n-len 1)
     (progn
      (setq dh (* (- (nth 1 l-h) h) (/ scale blh)))
      (setq p0 (polar p0 pi27 dh))
     ) ;progn end
    ) ;
    (setq p0 (polar p0 0 l))
  )
  ) ;if end
 (if (> n-len 1) 
 (progn 
  (setq m 1)
  (repeat (- n-len 2)
   (setq l (nth m l-l) h (nth m l-h) m (1+ m) )
   (draw_beam  l h p0)
   (if (= j-zc "1") (llm_dash p0))
    (if (= j-wz 0) 
        (llm_dash (polar p0 pi09 (+ hh (* 20 scale))))
    ) ;if end
   (setq dh (* (- (nth m l-h) h) (/ scale blh)))
   (setq p0 (polar p0 pi27 dh)
         p0 (polar p0 0 l)
   ) ;setq end
  ) ;repeat end
  (setq l (nth (1- n-len) l-l) 
        h (nth (1- n-len) l-h)
  ) ;setq end
  (if (< l 0.0)
   (progn
    (setq l (abs l))
    (setq direc 2)
    (draw_tl  l h p0 direc)
    (if (= j-zc "1") (llm_dash p0))
    (if (= j-wz 0) 
        (llm_dash (polar p0 pi09 (+ hh (* 20 scale))))
    ) ;if end
   ) ;progn end
   (progn
    (setq direc 2)
    (draw_beam  l h p0)
    (if (= j-zc "1") (llm_dash p0))
    (if (= j-wz 0) 
        (llm_dash (polar p0 pi09 (+ hh (* 20 scale))))
    ) ;if end
    (setq p0 (polar p0 0 l))
    (if (= j-zc "1") (llm_dash p0))
    (if (= j-wz 0) 
        (llm_dash (polar p0 pi09 (+ hh (* 20 scale))))
    ) ;if end
    (draw_dt  l h p0 direc)
   ) ;progn end
  ) ;if end
 ) ;progn end
 (progn 
  (setq direc 2)
  (draw_dt  l h p0 direc)
  (if (= j-zc "1") (llm_dash p0))
    (if (= j-wz 0) 
        (llm_dash (polar p0 pi09 (+ hh (* 20 scale))))
    ) ;if end
 ) ;progn end
) ;if end
(command "layer" "s" "0" "")
 (resvar) ;还原系统变量
 (princ)
) ;defun end


       


