;-----------------------------+
;独立基础（阶梯形）绘制图形   +
;-----------------------------+

(VMON)
;
(defun jc1_dash(p_dash b)
 (setq p_p1 (polar p_dash pi18 (+ (/ b 2) (* 2 scale)))
       delta (/ b 3.0)
       p_p2 (polar p_p1 0 (+ (* 2 scale) delta))
       p_p3 (polar p_p2 0 (/ delta 2.0))
       p_p3 (polar p_p3 (* pi09 0.5) (* 6 scale))
       p_p5 (polar p_p2 0 delta)
       p_p4 (polar p_p5 pi18 (/ delta 2.0))
       p_p4 (polar p_p4 (* pi09 -1.5) (* 6 scale))
       p_p6 (polar p_p5 0 (+ delta (* 2 scale)))
 ) ;setq end
 (command "line" p_p1 p_p2 p_p3 p_p4 p_p5 p_p6 "")
) ;defun end
;
(defun intp(ntype p0 p1 R)
 (if (< (distance p0 p1) R)
     (progn
          (setq xx0 (nth 0 p0)
                yy0 (nth 1 p0)
                x  (nth 0 p1)
                y  (nth 1 p1)
                RR (* R R)
                ydd (* (- y yy0) (- y yy0))
                xdd (* (- x xx0) (- x xx0))
                xd (sqrt (- RR ydd))          
                yd (sqrt (- RR xdd))
          ) ;setq end
          ;-----------
          ;(princ "\np0:") (princ x) (princ " , ") (princ y)
          ;---------
          (setq x (if (= ntype 1) (+ xx0 xd) x)
                y (if (= ntype 2) (+ yy0 yd) y)
          )      
          ;-----------
          ;(princ "\np:") (princ x) (princ " , ") (princ y)
          ;-----------
          (setq p (list x y)
          ) ;setq end
     ) ;progn end
     (setq p p1)
 ) ;if end
) ;defun end
;
(defun pim(p0 a1 a2 a3 a4 b1 b2 b3 b4 j1 j2 )
       (command "layer" "s" "0" "")
       (setq x0 (nth 0 p0)
             y0 (nth 1 p0)
             pi09 (* pi 0.5)
             pi18 pi
             pi27 (* pi 1.5)
             AA2 (+ a1 a2 a3 a4)
             BB2 (+ b1 b2 b3 b4)
             A (* 2 aa2)
             B (* 2 bb2)
             R (if (> AA2 BB2) BB2 AA2)
             R (* 0.7 R)
       ) ;setq end    
       (setq x41 (- x0 AA2)
             y41 (- y0 BB2)
             p41 (list x41 y41)
             p42 (polar p41 0 A)
             p43 (polar p42 pi09 B)
             p44 (polar p43 pi18 A)
       ) ;setq end
       (command "line" p41 p42 p43 p44 "c")
       (setq l_A (list a1 a2 a3)
             l_B (list b1 b2 b3)  
             ll1 0   
             ll2 0
             nn 0
       ) ;setq end      
       (repeat 3
          (setq ll1 (+ ll1 (nth nn l_A))
                ll2 (+ ll2 (nth nn l_B))
                nn (1+ nn)
                x11 (- x0 ll1)
                y11 (- y0 ll2)
                l1 (* 2 ll1)
                l2 (* 2 ll2)
                p110 (list x11 y11)
                p11 (intp 1 p41 p110 R)
                p12 (polar p110 0 l1)
                p13 (polar p12 pi09 l2)
                p14 (polar p13 pi18 l1)
                p150 (polar p14 pi27 l2)
                p15 (intp 2 p41 p150 R)
            ) ;setq end
            (command "line" p11 p12 p13 p14 p15 "")
       ) ;repeat end
; draw arc       
       (setq p1 (polar p41 0 R)
             p2 (polar p41 pi09 R)
       )
       (command "arc" "c" p41 p1 p2)
; draw reinforcement       
       (command "layer" "m" "pdim" "c" "green" "" "")
       (command "layer" "m" "rei" "c" "1" "" "")
       (setq scale0 (getvar "userr1")
             dx (* 1 scale0)
             dy dx
             xr0 (+ x41 dx)
             yr0 (+ y41 dy)
             n (fix (/ (- R dx) (* 5 scale0)))
             index 0
        ) ;setq end     
        (repeat (1+ n)
            (setq dx (* index (* 5 scale0))
                  dy dx
                  index (1+ index)
                  x1 (+ xr0 dx)
                  y1 (+ yr0 dy)
                  po1 (list x1 yr0)
                  po2 (intp 2 p41 po1 R)
                  po3 (list xr0 y1)
                  po4 (intp 1 p41 po3 R)
            ) ;setq end
            (command "line" po1 po2 "")
            ;-------
            ;(princ "\n draw rei line")
            ;(princ index) (getstring "\npress any key to continue")
            ;------
            (command "line" po3 po4 "")
        ) ;repeat end
;draw rei num1
        (command "layer" "s" "0" "")
        (setq dx (* 2.5 scale0)
              dy dx
              pr0 (list xr0 yr0)
              p1 (polar pr0 0 dx)
              p1 (polar p1 pi27 (* 10 scale0))
              p2 (polar po3 0 dx)
              p2 (polar p2 pi09 (* 2 scale0))
              p3 (polar p1 pi18 (* 20 scale0))
              pc (polar p3 pi18 (* 4 scale0))
              pr (* 4 scale0)
              htext (* 4 scale0)
              pj (polar p3 0 (* scale0 2))
              pj (polar pj pi09 (* scale0 2))
         ) ;setq end
         (command "line" p2 p1 p3 "")
         (command "circle" pc pr)
         (command "text" "j" "m" pc htext 0 "2")
         (command "text" pj htext 0 j2) 
;draw rei num2
        (setq dx (* 2.5 scale0)
              dy dx
              pr0 (list xr0 yr0)
              p1 (polar pr0 pi09 dy)
              p1 (polar p1 pi18 (* 10 scale0))
              p2 (polar po1 90 dx)
              p2 (polar p2 0 (* 2 scale0))
              p3 (polar p1 pi09 (* 20 scale0))
              pc (polar p3 pi09 (* 4 scale0))
              pj (polar p1 pi09 (* scale0 2))
              pj (polar pj pi18 (* scale0 2))
         ) ;setq end
         (command "line" p2 p1 p3 "")
         (command "circle" pc pr)
         (command "text" "j" "m" pc htext 90 "1")
         (command "text" pj htext 90 j1) 
;draw axis
         (setq p1 (polar p0 0 (+ aa2 (* 55 scale0)))
               p2 (polar p0 pi18 (+ aa2 (* 25 scale0)))
               p3 (polar p0 pi27 (+ bb2 (* 42 scale0)))
               p4 (polar p0 pi09 (+ bb2 (* 15 scale0)))
               rr (* 5 scale0)
               pc1 (polar p1 0 rr)  
               pc2 (polar p3 pi27 rr)
          ) ;setq end
          (command "line" p1 p2 "")
          (command "change" "l" "" "p" "lt" "axis" "")
          (command "line" p3 p4 "")
          (command "change" "l" "" "p" "lt" "axis" "")
          (command "circle" pc1 rr)          
          (command "circle" pc2 rr)
;draw dimension
         (command "layer" "s" "pdim" "")
         (setvar "dimdli" 0)
         ;(princ "\ndim1")
         (setq d1 (polar p42 0 (* 10 scale0))
               d2 (polar d1 pi09 b4)
               d3 (polar d2 pi09 b3)
               d4 (polar d3 pi09 b2)
               d5 (polar d4 pi09 b1)
               d6 (polar d5 pi09 b1)
               d7 (polar d6 pi09 b2)
               d8 (polar d7 pi09 b3)
               d9 (polar d8 pi09 b4)
               dp (polar d1 0 (* 10 scale0))
         )
         (if (/= b4 0)
            (command "dim" "ver" d1 d2 dp "" "con" d3 "" "con" d4 ""
                     "con" d5 "" "con" d6 "" "con" d7 "" "con" d8 ""
                     "con" d9 "" "e")
            (command "dim" "ver" d2 d3 dp ""  "con" d4 ""
                     "con" d5 "" "con" d6 "" "con" d7 "" "con" d8 "" "e")
         ) ;IF END
        ;dim2
        ;(princ "\ndim2")
        (setq d (+ A (* 3 scale0))
              d1 (polar dp 0 (* 10 scale0))
              d2 (polar d1 pi09 bb2)
              d3 (polar d2 pi09 bb2)
              dp (polar d1 0 (* 10 scale0))
         ) ;setq end
         (command "dim" "ver" d1 d2 dp "" "con" d3 "" "e")
         ;dim3
         ;(princ "\n dim3")
         (setq d1 (polar p41 pi27 (* 25 scale0))
               d2 (polar d1 0 AA2)
               d3 (polar d2 0 AA2)
               dp (polar d1 pi27 (* 10 scale0))
          ) ;setq end
          (command "dim" "hor" d1 d2 dp "" "con" d3 "" "e")
          (setq d1 (polar p41 pi27 (* 10 scale0))
                d2 (polar d1 0 a4)
                d3 (polar d2 0 a3)
                d4 (polar d3 0 a2)
                d5 (polar d4 0 a1)
                d6 (polar d5 0 a1)
                d7 (polar d6 0 a2)
                d8 (polar d7 0 a3)
                d9 (polar d8 0 a4)
                dp (polar d1 pi27 (* 10 scale0))
           ) ;setq end
           ;dim end
           (if (/= a4 0)
            (command "dim" "hor" d1 d2 dp "" "con" d3 "" "con" d4 ""
                     "con" d5 "" "con" d6 "" "con" d7 "" "con" d8 ""
                     "con" d9 "" "e")
            (command "dim" "hor" d2 d3 dp ""  "con" d4 ""
                     "con" d5 "" "con" d6 "" "con" d7 "" "con" d8 "" "e")
           ) ;if end
   (command "layer" "s" "0" "")
) ;defun end
;      
(defun  pom(p0 z1 z2 z3 a1 a2 a3 a4 bg bh)
       (jc1_dash p0 (* 2 a1))
       (setq x0 (nth 0 p0)
             y0 (nth 1 p0)
             pi09 (* pi 0.5)
             pi18 pi
             pi27 (* pi 1.5)
             A (* (+ a1 a2 a3 a4) 2)
             scale0 (getvar "userr1")
        ) ;setq end
        (setq p1 (polar p0 pi18 a1)
              p2 (polar p1 pi27 (* 30 scale0))              
              p3 (polar p2 pi18 a2)
              p4 (polar p3 pi27 z1)
              p5 (polar p4 pi18 a3)
              p6 (polar p5 pi27 z2)
              p7 (polar p6 pi18 a4)
              p8 (polar p7 pi27 z3)
              factor (/ 1.0 (getvar "dimlfac"))
              p9 (polar p8 pi18 (* 100 factor))
              p10 (polar p9 pi27 (* 100 factor))
              p11 (polar p9 0 (+ (* 100 factor) a1 a2 a3 a4))
              p12 (polar p10 0 (+ (* 100 factor) a1 a2 a3 a4))
        ) ;setq end
        (setvar "dimdli" 0)
        (command "layer" "s" "0" "")
        (command "line" p1 p2 p3 p4 p5 p6 p7 p8 "")
        (command "line" p11 p9 p10 p12 "")
        (command "layer" "s" "pdim" "")
        (setq d1 (polar p10 pi18 (* 3 scale0))
              d2 (polar d1 pi09 (* 100 factor))
              d3 (polar d2 pi09 z3)
              d4 (polar d3 pi09 z2)
              d5 (polar d4 pi09 z1)
              d6 (polar d5 pi09 (* 15 scale0))
              dp (polar d1 pi18 (* 10 scale0))
         )
         (if (/= z3 0)
          (command "dim" "ver" d1 d2 dp "" "con" d3 "" "con" d4 "" "con" d5 "" "con" d6 "40d" "e")
          (command "dim" "ver" d1 d2 dp "" "con" d4 "" "con" d5 "" "con" d6 "40d" "e")
         ) ;if end
         (setq p_bg (polar p9 pi18 (* 25 scale0))
               sc_x (/ scale0 100.0)
               sc_y sc_x
         )
         (command "insert" "elul" p_bg sc_x sc_y 0 bg)
         (setq p13 (polar p8 0 (* 2.0 scale0))
               p13 (polar p13 pi09 (* 3.0 scale0))
               p14 (polar p11 pi09 (* 3.0 scale0))
               p15 (polar p1 pi27 (* 15 scale0))
               p15 (polar p15 0 (* 1.5 scale0))
               p16 (polar p14 pi09 (* 1.5 scale0))
               p16 (polar p16 pi18 (- a1 (* 1.5 scale0)))
               p17 (polar p16 pi18 (* 10 scale0))
               p18 (polar p15 0 (- a1 (* 1.5 scale0)))
               p19 (polar p15 (* pi09 -0.5) (* 2 scale0))
               p20 (polar p19 pi09 (* 10 scale0))
               p21 (polar p20 0 (+ (* a1 2) (* 20 scale0)))
               txt_1 "基础插筋"
               txt_2 "规格同柱主筋"
               h_txt (* 4 scale0)
               p1_txt (polar p20 0 (+ (* a1 2) (* 4 scale0)))
               p2_txt (polar p1_txt pi27 (* 6 scale0))
               p1_txt (polar p1_txt pi09 (* 2 scale0))
         ) ;setq end
         (command "layer" "s" "rei" "")
         (command "line" p13 p14 "")
         (command "line" p15 p16 p17 "") 
         (command "line" p14 p18 "")
;         
         (command "line" p15 p19 "")
         (command "layer" "s" "0" "")
         (command "line" p19 p20 p21 "")
         (command "mirror" "c" p10 p18 "" p11 p18 "n")
         (command "text" p1_txt h_txt 0 txt_1) 
         (command "text" p2_txt h_txt 0 txt_2)
;         
         (setq d1 (polar p10 pi27 (* 2.0 scale0))
               d2 (polar d1 0 (* 100 factor))
               dp (polar d1 pi27 (* 10 scale0))
               pdo (polar p13 pi27 (* 1.0 scale0))
               xd (* 5 scale0)
               rd (* 1 scale0)
         ) ;setq end
         (command "layer" "s" "pdim" "")
         (command "dim1" "hor" d1 d2 dp "")
         (command "layer" "s" "0" "")
         (command "donut" 0 rd pdo "")
         (command "array" "l" "" "r" 1 6 xd)
         (setq p1 (polar pdo 0 (* 15 scale0))
               p2 (polar p1 pi27 (* 15 scale0))
               pc (polar p2 pi27 (* 4 scale0))
               rr (* 4.0 scale0)
               htext (* 4 scale0)
         )
         (command "line" p1 p2 "")
         (command "circle" pc rr)
         (command "text" "j" "m" pc htext 0 "1")
         (setq p1 (polar p13 0 (* 0.7 A))
               p2 (polar p1 pi27 (* 15 scale0))
               pc (polar p2 pi27 (* 4 scale0))
         )
         (command "line" p1 p2 "")
         (command "circle" pc rr)
         (command "text" "j" "m" pc htext 0 "2")
         (setq p1 (polar p12 pi27 (* 10 scale0))
               p2 (polar p0 pi09 (* 5 scale0))
               pc (polar p1 pi27 (* 5 scale0))
               rr (* 5.0 scale0)
         )
         (command "line" p1 p2 "")
         (command "circle" pc rr)
         (setq pbh (polar pc pi27 (* 13 scale0))     
               pl1 (polar pbh pi27 (* 2 scale0))
               pl1 (polar pl1 pi18 (* 10 scale0))
               pl2 (polar pl1 0 (* 20 scale0))
               htext (* 7 scale0)  
         )
         (command "text" "j" "c" pbh htext 0 bh)
         (command "layer" "s" "rei" "")
         (command "line" pl1 pl2 "")
         (command "layer" "s" "0" "")
) ;defun end
;--------
;
(defun C:JC1()
 (inivar) ;初始化系统变量
 (setq scale0 (getvar "userr1"))
 (setq l_bg '()
       l_a1 '()
       l_a2 '()
       l_a3 '()
       l_a4 '()
       l_b1 '()
       l_b2 '()
       l_b3 '()
       l_b4 '()
       l_z1 '()
       l_z2 '()
       l_z3 '()
       l_j1 '()
       l_j2 '()
       l_tx '("\n标高:(m)" "\na1:(mm)" "\na2:(mm)" "\na3:(mm)" "\na4:(mm)" "\nb1:(mm)" "\nb2:(mm)" "\nb3:(mm)" "\nb4:(mm)" "\nz1:(mm)" "\nz2:(mm)" "\nz3:(mm)" "\n配筋1:" "\n配筋2:") 
 ) ;setq end
 (command "layer" "s" "0" "")
 (princ "\n输入基础表格插入点:")
 (command "insert" "T-JC1" "x" scale0 "y" scale0 "r" 0 pause)
 (setq p0 (getvar "lastpoint")
       x0 (nth 0 p0)
       y0 (nth 1 p0)
       xp2 (+ x0 (* scale0 320))
       yp2 (+ y0 (* scale0 180))
       p2 (list xp2 yp2)
       iy 1   
       temp T) ;setq end
 (command "zoom" "w" p0 p2)
 (while temp
       (setq ix 1
             dx (if (> ix 12) (* 25 scale0) (* 10 scale0))
             dy (* 10 scale0)
       )
       (setq xtext (+ x0 (* 12.0 scale0) (* (1- ix) dx)) 
             ytext (+ y0 (* (- 10 iy) dy) (* 2.0 scale0))
             ptext (list xtext ytext)
             htext (* 3.0 scale0)
       ) ;setq end
       (princ "\n请输入参数,每次一个,如只有两级台阶,a4,b4,z3应输0!")
       (PRINC "\n如所有参数都已输完,按回车键继续")
       ;(getstring "\n击任意键继续")
       (setq ttxt (nth (1- ix) l_tx))
       (princ ttxt)
       (setq txt (getstring))
       (command "text" ptext htext 0 txt) 
       (if (= txt "") 
           (setq temp nil)
           (progn
            (setq ix (1+ ix)
                  l_bg (cons txt l_bg)
            ) 
            (while (<= ix 14)  ;while1
             (setq dx (if (> ix 12) (* 25 scale0) (* 10 scale0))
                   xtext (+ x0 (* 12.0 scale0) (* 10.0 (1- ix) scale0)) 
                   xtext (if (> ix 13) (+ xtext (* 15 scale0)) xtext)
                   ytext (+ y0 (* 10.0 (- 10.0 iy) scale0) (* 2.0 scale0))
                   ptext (list xtext ytext)
                   htext (* 3.0 scale0)
             ) ;setq end
             (setq ttxt (nth (1- ix) l_tx))
             (princ ttxt)
             (setq txt (getstring))
             (command "text" ptext htext 0 txt)
             (if (and (= txt "") (<= ix 14) )
              (princ "\n请把这个基础参数输完")
              (progn  ;progn1
               (cond
                 ((= ix 2) (setq l_a1 (cons (atof txt) l_a1))) 
                 ((= ix 3) (setq l_a2 (cons (atof txt) l_a2))) 
                 ((= ix 4) (setq l_a3 (cons (atof txt) l_a3))) 
                 ((= ix 5) (setq l_a4 (cons (atof txt) l_a4))) 
                 ((= ix 6) (setq l_b1 (cons (atof txt) l_b1))) 
                 ((= ix 7) (setq l_b2 (cons (atof txt) l_b2))) 
                 ((= ix 8) (setq l_b3 (cons (atof txt) l_b3))) 
                 ((= ix 9) (setq l_b4 (cons (atof txt) l_b4))) 
                 ((= ix 10) (setq l_z1 (cons (atof txt) l_z1))) 
                 ((= ix 11) (setq l_z2 (cons (atof txt) l_z2))) 
                 ((= ix 12) (setq l_z3 (cons (atof txt) l_z3))) 
                 ((= ix 13) (setq l_j1 (cons txt l_j1))) 
                 ((= ix 14) (setq l_j2 (cons txt l_j2))) 
               ) ;cond end                 
               (setq ix (1+ ix))
              ) ;progn1 end
             ) ;if end
            ) ;while1 end
           )  ;progn end
       )      ;if end  
       (setq iy (1+ iy))
   ) ;while end
   (setq l_bg (reverse l_bg)
         l_a1 (reverse l_a1)
         l_a2 (reverse l_a2)
         l_a3 (reverse l_a3)
         l_a4 (reverse l_a4)
         l_b1 (reverse l_b1)
         l_b2 (reverse l_b2)
         l_b3 (reverse l_b3)
         l_b4 (reverse l_b4)
         l_z1 (reverse l_z1)
         l_z2 (reverse l_z2)
         l_z3 (reverse l_z3)
         l_j1 (reverse l_j1)
         l_j2 (reverse l_j2)
    ) ;setq end
   (command "zoom" "p")
   (command "erase" "w" p0 p2 "")
   ;table end
   ;
   ;draw foundation
   (setq n_jc1 (length l_a1)
         n_jc 0
   ) 
   (repeat n_jc1
      (setq a1 (nth n_jc l_a1)
            a2 (nth n_jc l_a2)
            a3 (nth n_jc l_a3)
            a4 (nth n_jc l_a4)
            b1 (nth n_jc l_b1)
            b2 (nth n_jc l_b2)
            b3 (nth n_jc l_b3)
            b4 (nth n_jc l_b4)
            z1 (nth n_jc l_z1)
            z2 (nth n_jc l_z2)
            z3 (nth n_jc l_z3)
            bg (nth n_jc l_bg)
            j1 (nth n_jc l_j1)
            j2 (nth n_jc l_j2)
            bh (strcat "J-" (itoa (1+ n_jc)))
            n_jc (1+ n_jc)
      ) ;setq end           
      (setq scale0 (getvar "userr1"))
      (princ "\n局部比例:<")
      (princ scale0)
      (setq txt (getstring ">"))
      (if (/= txt "")
          (setq scale1 (atoi txt))
          (setq scale1 scale0)
      ) ;if end
      (setq diml (getvar "DIMLFAC"))
      (setvar "DIMLFAC" (/ scale1 scale0))
      (setq factor (/ scale0 scale1)
            a1 (* a1 factor)
            a2 (* a2 factor)
            a3 (* a3 factor)
            a4 (* a4 factor)
            b1 (* b1 factor)
            b2 (* b2 factor)
            b3 (* b3 factor)
            b4 (* b4 factor)
            z1 (* z1 factor)
            z2 (* z2 factor)
            z3 (* z3 factor)
            A (* 2 (+ a1 a2 a3 a4) )
            B (* 2 (+ b1 b2 b3 b4) )
            Z (+ z1 z2 z3) 
            l1 (+ (* scale0 45) A)
            l2 (+ (* scale0 120) Z B (* factor 100))
      ) ;setq end
      (princ (strcat "\n基础J-" (itoa n_jc) "插入点:"))
      (command "insert" "border" "x" l1 "y" l2 "r" 0 pause)
      (setq p01 (getvar "lastpoint")
            x01 (nth 0 p01)
            y01 (nth 1 p01)
            x02 (+ x01 (* scale0 30) (/ A 2))
            y02 (+ y01 (* scale0 105) (/ B 2) Z (* factor 100) )
            p02 (list x02 y02)
            x03 x02
            y03 (+ y01 (* scale0 40) Z (* factor 100) )
            p03 (list x03 y03)
      ) ;setq end
      (command "erase" "l" "")
      (pim p02 a1 a2 a3 a4 b1 b2 b3 b4 j1 j2)
      (pom p03 z1 z2 z3 a1 a2 a3 a4 bg bh)
   ) ;repeat end
   (setvar "dimlfac" 1.0)
   (command "redraw")
   (command "layer" "s" "0" "")
 (resvar) ;还原系统变量
 (princ)
) ;JC1 end
