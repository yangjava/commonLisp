(VMON)
(command "layer" "m" "rei" "c" "1" "" "s" "0" "")
(command "layer" "m" "pdim" "c" "green" "" "s" "0" "")
(setq scale (getvar "userr1")
      scale1 scale
      factor 1
) ;setq  end
;
;--------
;
(defun C:DRAW_CT()
 (setvar "blipmode" 0)
 (setvar "cmdecho" 0)
 (setq oer *error* *error* clmerr)
 (setq scale0 (getvar "userr1"))
 (setvar "dimlfac" (/ 1.0 factor))
   ;
     (setq  l1 (+ (* scale0 50) (* 2 d_s))
            l2 (+ (* scale0 140) (* 2 d_s))
      ) ;setq end
      (princ (strcat "\n承台" "插入点:"))
      (command "insert" "border" "x" l1 "y" l2 "r" 0 pause)
      (setq p01 (getvar "lastpoint")
            x01 (nth 0 p01)
            y01 (nth 1 p01)
            x03 (+ x01 (* scale0 50) (/ d_s 2.0))
            y03 (+ y01 (* scale0 50) d_h (* factor 100) )
            pp03 (list x03 y03)
            x02 x03
            y02 (+ y03 (* scale0 120))
            pp02 (list x02 y02)
      ) ;setq end
      (command "erase" "l" "")
    (cond 
         ((= n_type  1)
            (progn
                (setq d_ss d_s d_s 0)
                (pim_ct2 pp02 d_pj1 d_pj2)
                (pom_ct2 pp03 d_bg )
                (setq d_s d_ss)
             ); progn end
            ) ;condition end
         ((= n_type  2)
            (progn
                (pim_ct2 pp02 d_pj1 d_pj2)
                (pom_ct2 pp03 d_bg)
             ); progn end
            ) ;condition end
         ((= n_type  3)
            (progn
                (pim_ct3 pp02 d_pj1 d_pj2)
                (pom_ct3 pp03 d_bg)
             ); progn end
            ) ;condition end
         ((= n_type  4)
            (progn
                (pim_ct4 pp02 d_pj1 d_pj2)
                (pom_ct2 pp03 d_bg)
             ); progn end
            ) ;condition end
         ((= n_type  5)
            (progn
                (setq d_ss d_s 
                      d_s (* d_s (sqrt 2.0))
                )
                (pim_ct5 pp02 d_pj1 d_pj2)
                (pom_ct3 pp03 d_bg)
                (setq d_s d_ss)
             ); progn end
            ) ;condition end
         ((= n_type  6)
            (progn
                (pim_ct6 pp02 d_pj1 d_pj2)
                (setq d_s (* 2.0 d_s))
                (pom_ct3 pp03 d_bg)
                (Setq d_s (* 0.5 d_s))
             ); progn end
            ) ;condition end
        ) ;cond end
   (setvar "dimlfac" 1.0)
   (redraw)
   (setvar "cmdecho" 1)
   (command "layer" "s" "0" "")
 ) ;JC1 end
;
(defun C:JBBL()
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
      (setq factor (/ scale0 scale1))
) ;JBBL defun end
;
(defun C:CTCS()
 (setq d_d (getreal "\n圆桩直径:(mm)")
       d_d (* d_d factor)
       d_s (getreal "\n桩距:(mm)")
       d_s (* d_s factor)
       d_h (getreal "\n承台高:(mm)")
       d_h (* d_h factor)
       d_zb (getreal "\n柱宽:(mm)")
       d_zb (* d_zb factor)
       d_zh (getreal "\n柱高:(mm)")
       d_zh (* d_zh factor)
       d_bg (getstring "\n承台底标高:(m)")
       d_pj1 (getstring "\n主受力方向配筋:")
       d_pj2 (getstring "\n次受力方向配筋:")
 ) ;setq end       
) ;defun CTCS end
;        
;
(defun intp(ntype p0 p1 R / xx0 yy0 x y RR ydd xdd xd yd )
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
          ;---------
          (setq x (if (= ntype 1) (+ xx0 xd) x)
                y (if (= ntype 2) (+ yy0 yd) y)
                y (if (= ntype 3) (- yy0 yd) y)
          )      
          (setq p (list x y)
          ) ;setq end
     ) ;progn end
     (setq p p1)
 ) ;if end
) ;defun end
;      
(defun  pom_ct3(p0 bg)
       (setq x0 (nth 0 p0)
             y0 (nth 1 p0)
             pi09 (* pi 0.5)
             pi18 pi
             pi27 (* pi 1.5)
             scale0 (getvar "userr1")
        ) ;setq end
        (setq p1 (polar p0 pi18 (+ d_d (/ d_s 2.0)))
              p2 (polar p1 pi27 d_h)
;              factor (/ 1.0 (getvar "dimlfac"))
              d100 (* 100 factor)
              p3 (polar p2 0 (/ d_d 2.0))
              p4 (polar p2 pi18 d100)
              p5 (polar p4 pi27 d100)
              p6 (polar p5 0 (+ (/ d_d 2.0) d100))
        ) ;setq end
        (setvar "dimdli" 0)
        (command "layer" "s" "0" "")
        (command "line" p0 p1 p2 "")
        (command "line" p3 p4 p5 p6 "")
        (setq p7 (Polar p3 pi09 d100)
              p8 (polar p7 pi27 (* 30 scale))
              p9 (polar p7 0 d_d)
              p10 (polar p8 0 d_d)
        ) ;setq end
        (setq ss (ssadd))
        (command "line" p8 p7 "")
            (setq ename (entlast)
                  ss (ssadd ename ss)
            ) ;setq end
        (command "line" p7 p9 "")      
            (setq ename (entlast)
                  ss (ssadd ename ss)
            ) ;setq end
        (command "line" p9 p10 "")
        (setq p11 (polar p9 pi27 d100)
              p12 (polar p11 0 (- (/ d_s 2.0) (/ d_d 2.0)))
              p13 (polar p11 pi27 d100)
              p14 (polar p12 pi27 d100)
        ) ;setq end
        (command "line" p11 p12 "")
        (command "line" p13 p14 "")
        (setq pr1 (polar p7 0 (* 1.0 scale))
              pr2 (polar p8 0 (* 1.0 scale))
              pr3 (polar p9 pi18 (* 1.0 scale))
              pr4 (polar p10 pi18 (* 1.0 scale))
              pr5 (polar pr1 (/ pi 1.5) (* 15 scale))
              pr6 (polar pr3 (/ pi 3.0) (* 15 scale))
        ) ;setq end
        (command "layer" "s" "rei" "")
        (command "line" pr5 pr1 "")
            (setq ename (entlast)
                  ss (ssadd ename ss)
            ) ;setq end
        (command "line" pr1 pr2 "")
            (setq ename (entlast)
                  ss (ssadd ename ss)
            ) ;setq end
        (command "line" pr6 pr3 pr4 "")
        (setq pr11 (polar p2 pi09 (+ d100 scale))
              pr1 (polar pr11 0 (* 1.0 scale))
              pr2 (Polar pr11 0 (+ (/ d_s 2.0) d_d))
        ) ;setq end
        (command "line" pr1 pr2 "")
        (setq pr11 (polar p1 pi27 (* 1.0 scale))
              pr1 (polar pr11 0 (* 1.0 scale))
              pr2 (Polar pr11 0 (+ (/ d_s 2.0) d_d))
        ) ;setq end
        (command "line" pr1 pr2 "")
        (setq pr1 (polar p0 pi18 (- (/ d_zh 2.0) (* 1.0 scale)))
              pr2 (polar pr1 pi27 (- d_h d100 (* 3.0 scale)))
              pr3 (polar pr2 pi18 (* 5 scale))
              pr1 (polar pr1 pi09 (* 20 scale))
              pr4 (polar pr1 (* -0.25 pi) (* 2.0 scale))
        ) ;setq end
        (command "line" pr3 pr2 pr1 pr4 "")
        (setq ptxt1 pr4
              ptxt2 (polar ptxt1 pi09 (* 15 scale))
              ptxt3 (polar ptxt2 0 (+ d_zh (* 30 scale)))
              ptext (polar ptxt2 0 (+ d_zh (* 3 scale)))
              ptext1 (Polar ptext pi09 (* 1.0 scale))
              ptetx2 (Polar ptext1 pi27 (* 6.0 scale))
              htext (* 4.0 scale)
              txt1 "基础插筋"
              txt2 "规格同柱主筋"
         ) ;setq end
         (command "layer" "s" "0" "")
         (command "line" ptxt1 ptxt2 ptxt3 "")
         (command "text" ptext1 htext 0 txt1)
         (command "text" ptetx2 htext 0 txt2)
         (command "layer" "s" "rei" "")
         (setq pr1 (polar pr2 pi09 (* scale 1.0))
              pr2 (polar pr1 0 (- (/ d_zh 2.0) scale))
              pr3 (polar pr1 pi09 (- d_h d100 (* 6.0 scale)))
              pr4 (Polar pr3 0 (- (/ d_zh 2.0) scale))
        ) ;setq end
        (command "line" pr1 pr2 "")
        (command "line" pr3 pr4 "")
        (command "layer" "s" "0" "")
        (Setq zp1 (polar p8 pi18 (+ d_d d100))
              zp2 (polar p0 pi09 (* 20 scale))
        ) ;setq end
        (setq txt (strcat  (rtos (/ d_s 2.0)) ",0" ))
        (command "copy" ss "" txt "")
        (command "mirror" "c" zp1 zp2 "" p0 p12 "n")
        (setq d1 (polar p5 pi18 (* 3 scale0))
              d2 (polar d1 pi09 d100)
              d3 (polar d2 pi09 d100)
              dp (polar d1 pi18 (* 10 scale0))
         )
         (command "layer" "s" "pdim" "")
         (setvar "dimlfac" (/ 1.0 factor))
         (command "dim" "ver" d1 d2 dp "" "con" d3 "" "e")
         (setq d1 (polar d2 pi18 (* 10 scale0))
               d2 (polar d1 pi09 d_h)
               d3 (polar d2 pi09 (* 20 scale))
               dp (polar d1 pi18 (* 10 scale))
         ) ;setq end
         (command "dim" "ver" d1 d2 dp "" "con" d3 "40d" "e")
         (setq p_bg (polar p4 pi18 (* 35 scale0))
               sc_x (/ scale0 100.0)
               sc_y sc_x
         )
         (command "insert" "elul" p_bg sc_x sc_y 0 bg)
         (setq p1 (polar p0 pi27 (+ d_h d100 (* 55 scale)))
               c_r (* 4.0 scale)
               pc (polar p1 pi27 c_r)
         ) ;setq end
         (command "line" p0 p1 "")
         (command "change" "l" "" "p" "lt" "axis" "")
         (command "circle" pc c_r)
         (setq d4 (polar p1 pi09 (* 15 scale))
               d3 (polar d4 pi18 (/ d_s 2.0))
               d2 (polar d3 pi18 d_d)
               d1 (polar d2 pi18 d100)
               d5 (polar d4 0 (/ d_s 2.0))
               d6 (polar d5 0 d_d)
               d7 (polar d6 0 d100)
               dp (polar d1 pi27 (* 10 scale))  
         ) ;setq end      
         (command "dim" "hor" d1 d2 dp "" "con" d3 "" "con" d4 ""
                  "con" d5 "" "con" d6 "" "con" d7 "" "e")
         (command "layer" "s" "0" "")
) ;defun end
;
(defun pim_ct2(p0 j1 j2 )
       (command "layer" "s" "0" "")
       (setq x0 (nth 0 p0)
             y0 (nth 1 p0)
             pi09 (* pi 0.5)
             pi18 pi
             pi27 (* pi 1.5)
             B/2 (/ d_zb 2.0)
             H/2 (/ d_zh 2.0)
             D/2 (/ d_d 2.0)
             R (* 1.8 d_d)
       ) ;setq end    
       (setq p41 (polar p0 pi18 d_d)
             p42 (Polar p41 0 (* d_d 2.0))
             p43 (polar p42 pi09 (+ (* 2.0 d_d) d_s))
             p44 (polar p43 pi18 (* d_D 2.0))
       ) ;setq end
       (command "line" p41 p42 p43 p44 "c")
       (setq p0 (Polar p0 pi09 (+ d_d (/ d_s 2.0)))
             p11 (polar p0 pi27 H/2)   
             p11 (polar p11 pi18 B/2)
             p12 (polar p11 0 d_zb)
             p13 (polar p12 pi09 d_zh)
             p14 (polar p13 pi18 d_zb)
             p150 (polar p14 pi27 d_zh)
             p15 (intp 2 p41 p150 R)
       ) ;setq end      
       (command "line" p11 p12 p13 p14 p15 "")
; draw arc       
       (setq p1 (polar p41 0 R)
             p2 (polar p41 pi09 R)
       )
       (command "arc" "c" p41 p1 p2)
; draw reinforcement       
       (command "layer" "m" "rei" "c" "1" "" "")
       (setq scale0 (getvar "userr1")
             dx (* 2 scale0)
             dy dx
             x41 (nth 0 p41)
             y41 (nth 1 p41)
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
         (princ "\ndraw axis")
         (setq p1 (polar p0 0 (+ d_d (* 5 scale0)))
               p2 (polar p0 pi18 (+ d_d (* 55 scale0)))
               p3 (polar p0 pi27 (+ (/ d_s 2.0) d_d (* 55 scale0)))
               p4 (polar p0 pi09 (+ (/ d_s 2.0) d_d (* 5 scale0)))
               rr (* 5 scale0)
               pc1 (polar p2 pi18 rr)  
               pc2 (polar p3 pi27 rr)
          ) ;setq end
          (command "line" p1 p2 "")
          (command "change" "l" "" "p" "lt" "axis" "")
          (command "line" p3 p4 "")
          (command "change" "l" "" "p" "lt" "axis" "")
          (command "circle" pc1 rr)          
          (command "circle" pc2 rr)
;draw dimension
         (setvar "dimdli" 0)
         (command "layer" "s" "pdim" "")
         (princ "\ndim1")
         (setq d2 (polar p0 pi18 (+ d_d (* 20 scale0)))
               d1 (polar d2 pi27 H/2)
               d3 (Polar d2 pi09 H/2)
               dp (polar d1 pi18 (* 10 scale0))
         )
        (command "dim" "ver" d1 d2 dp "" "con" d3 "" "e")
        (Setq d1 (Polar p41 pi18 (* 30 scale0))
              d2 (polar d1 pi09 d_d)
              d3 (polar d2 pi09 (/ d_s 2.0))
              d4 (polar d3 pi09 (/ d_s 2.0))
              d5 (polar d4 pi09 d_d)
              dp (polar d1 pi18 (* 10 scale0))
        ) ;setq end
        (command "dim" "ver" d1 d2 dp "" "con" d3 "" "con" d4 ""
                 "con" d5 "" "e")
        (setq d1 dp
              d2 (Polar d3 pi18 (* 10 scale0))
              d3 (polar d5 pi18 (* 10 scale0))
              dp (polar d1 pi18 (* 10 scale0))
        ) ;setq end
        (command "dim" "ver" d1 d2 dp ""  "con" d3 "" "e")
         ;dim3
         (princ "\n dim3")
         (setq d2 (polar p41 0 d_d)
               d2 (polar d2 pi27 (* 20 scale0))
               d1 (polar d2 pi18 B/2)
               d3 (polar d2 0 B/2)
               dp (polar d1 pi27 (* 10 scale0))
          ) ;setq end
          (command "dim" "hor" d1 d2 dp "" "con" d3 "" "e")
          (Setq d1 (polar p41 pi27 (* 30 scale0))
                d2 (polar d1 0 d_d)
                d3 (polar d2 0 d_d)
                dp (polar d1 pi27 (* 10 scale0))
          ) ;setq end
          (command "dim" "hor" d1 d2 dp "" "con" d3 "" "e")
          (setq p01 (polar p0 pi09 (/ d_s 2.0))
                p02 (polar p0 pi27 (/ d_S 2.0))
          ) ;setq end
          (draw_cir p01 d_d)
          (draw_cir p02 d_d)
) ;defun end
;      
(defun  pom_ct2(p0 bg)
       (setq x0 (nth 0 p0)
             y0 (nth 1 p0)
             pi09 (* pi 0.5)
             pi18 pi
             pi27 (* pi 1.5)
             scale0 (getvar "userr1")
        ) ;setq end
        (setq p1 (polar p0 pi18 (+ d_d (/ d_s 2.0)))
              p2 (polar p1 pi27 d_h)
              ; factor (/ 1.0 (getvar "dimlfac"))
              d100 (* 100 factor)
              p3 (polar p2 0 (/ d_d 2.0))
              p4 (polar p2 pi18 d100)
              p5 (polar p4 pi27 d100)
              p6 (polar p5 0 (+ (/ d_d 2.0) d100))
        ) ;setq end
        (setvar "dimdli" 0)
        (command "layer" "s" "0" "")
        (command "line" p0 p1 p2 "")
        (command "line" p3 p4 p5 p6 "")
        (setq p7 (Polar p3 pi09 d100)
              p8 (polar p7 pi27 (* 30 scale))
              p9 (polar p7 0 d_d)
              p10 (polar p8 0 d_d)
        ) ;setq end
        (command "line" p8 p7 p9 p10 "")
        (setq p11 (polar p9 pi27 d100)
              p12 (polar p11 0 (- (/ d_s 2.0) (/ d_d 2.0)))
              p13 (polar p11 pi27 d100)
              p14 (polar p12 pi27 d100)
        ) ;setq end
        (command "line" p11 p12 "")
        (command "line" p13 p14 "")
        (setq pr1 (polar p7 0 (* 1.0 scale))
              pr2 (polar p8 0 (* 1.0 scale))
              pr3 (polar p9 pi18 (* 1.0 scale))
              pr4 (polar p10 pi18 (* 1.0 scale))
              pr5 (polar pr1 (/ pi 1.5) (* 15 scale))
              pr6 (polar pr3 (/ pi 3.0) (* 15 scale))
        ) ;setq end
        (command "layer" "s" "rei" "")
        (command "line" pr5 pr1 pr2 "")
        (command "line" pr6 pr3 pr4 "")
        (setq pr11 (polar p2 pi09 (+ d100 scale))
              pr1 (polar pr11 0 (* 1.0 scale))
              pr2 (Polar pr11 0 (+ (/ d_s 2.0) d_d))
        ) ;setq end
        (command "line" pr1 pr2 "")
        (setq pr11 (polar p1 pi27 (* 1.0 scale))
              pr1 (polar pr11 0 (* 1.0 scale))
              pr2 (Polar pr11 0 (+ (/ d_s 2.0) d_d))
        ) ;setq end
        (command "line" pr1 pr2 "")
        (setq pr1 (polar p0 pi18 (- (/ d_zh 2.0) (* 1.0 scale)))
              pr2 (polar pr1 pi27 (- d_h d100 (* 3.0 scale)))
              pr3 (polar pr2 pi18 (* 5 scale))
              pr1 (polar pr1 pi09 (* 20 scale))
              pr4 (polar pr1 (* -0.25 pi) (* 2.0 scale))
        ) ;setq end
        (command "line" pr3 pr2 pr1 pr4 "")
        (setq ptxt1 pr4
              ptxt2 (polar ptxt1 pi09 (* 15 scale))
              ptxt3 (polar ptxt2 0 (+ d_zh (* 30 scale)))
              ptext (polar ptxt2 0 (+ d_zh (* 3 scale)))
              ptext1 (Polar ptext pi09 (* 1.0 scale))
              ptetx2 (Polar ptext1 pi27 (* 6.0 scale))
              htext (* 4.0 scale)
              txt1 "基础插筋"
              txt2 "规格同柱主筋"
         ) ;setq end
         (command "layer" "s" "0" "")
         (command "line" ptxt1 ptxt2 ptxt3 "")
         (command "text" ptext1 htext 0 txt1)
         (command "text" ptetx2 htext 0 txt2)
         (command "layer" "s" "rei" "")
        (setq pr1 (polar pr2 pi09 (* scale 1.0))
              pr2 (polar pr1 0 (- (/ d_zh 2.0) scale))
              pr3 (polar pr1 pi09 (- d_h d100 (* 6.0 scale)))
              pr4 (Polar pr3 0 (- (/ d_zh 2.0) scale))
        ) ;setq end
        (command "line" pr1 pr2 "")
        (command "line" pr3 pr4 "")
        (command "layer" "s" "0" "")
        (Setq zp1 (polar p8 pi18 (+ d_d d100))
              zp2 (polar p0 pi09 (* 20 scale))
        ) ;setq end
        (command "mirror" "c" zp1 zp2 "" p0 p12 "n")
        (setq d1 (polar p5 pi18 (* 3 scale0))
              d2 (polar d1 pi09 d100)
              d3 (polar d2 pi09 d100)
              dp (polar d1 pi18 (* 10 scale0))
         )
         (command "layer" "s" "pdim" "")
         (command "dim" "ver" d1 d2 dp "" "con" d3 "" "e")
         (setq d1 (polar d2 pi18 (* 10 scale0))
               d2 (polar d1 pi09 d_h)
               d3 (polar d2 pi09 (* 20 scale))
               dp (polar d1 pi18 (* 10 scale))
         ) ;setq end
         (command "dim" "ver" d1 d2 dp "" "con" d3 "40d" "e")
         (setq p_bg (polar p4 pi18 (* 35 scale0))
               sc_x (/ scale0 100.0)
               sc_y sc_x
         )
         (command "insert" "elul" p_bg sc_x sc_y 0 bg)
         (setq p1 (polar p0 pi27 (+ d_h d100 (* 55 scale)))
               c_r (* 4.0 scale)
               pc (polar p1 pi27 c_r)
         ) ;setq end
         (command "line" p0 p1 "")
         (command "change" "l" "" "p" "lt" "axis" "")
         (command "circle" pc c_r)
         (setq d4 (polar p1 pi09 (* 15 scale))
               d3 (polar d4 pi18 (/ d_s 2.0))
               d2 (polar d3 pi18 d_d)
               d1 (polar d2 pi18 d100)
               d5 (polar d4 0 (/ d_s 2.0))
               d6 (polar d5 0 d_d)
               d7 (polar d6 0 d100)
               dp (polar d1 pi27 (* 10 scale))  
         ) ;setq end      
         (command "dim" "hor" d1 d2 dp "" "con" d3 "" "con" d4 ""
                  "con" d5 "" "con" d6 "" "con" d7 "" "e")
         (command "layer" "s" "0" "")
) ;defun end
;
(defun pim_ct4(p0 j1 j2 )
       (command "layer" "s" "0" "")
       (setq x0 (nth 0 p0)
             y0 (nth 1 p0)
             pi09 (* pi 0.5)
             pi18 pi
             pi27 (* pi 1.5)
             B/2 (/ d_zb 2.0)
             H/2 (/ d_zh 2.0)
             D/2 (/ d_d 2.0)
             R (* 1.8 d_d)
       ) ;setq end    
       (setq p41 (polar p0 pi18 (+ d_d (/ d_s 2.0)))
             p42 (Polar p41 0 (+ (* d_d 2.0) d_s))
             p43 (polar p42 pi09 (+ (* 2.0 d_d) d_s))
             p44 (polar p43 pi18 (+ (* d_D 2.0) d_s))
       ) ;setq end
       (command "line" p41 p42 p43 p44 "c")
       (setq p0 (Polar p0 pi09 (+ d_d (/ d_s 2.0)))
             p11 (polar p0 pi27 H/2)   
             p11 (polar p11 pi18 B/2)
             p12 (polar p11 0 d_zb)
             p13 (polar p12 pi09 d_zh)
             p14 (polar p13 pi18 d_zb)
             p150 (polar p14 pi27 d_zh)
             p15 (intp 2 p41 p150 R)
       ) ;setq end      
       (command "line" p11 p12 p13 p14 p15 "")
; draw arc       
       (setq p1 (polar p41 0 R)
             p2 (polar p41 pi09 R)
       )
       (command "arc" "c" p41 p1 p2)
; draw reinforcement       
       (command "layer" "m" "rei" "c" "1" "" "")
       (setq scale0 (getvar "userr1")
             dx (* 2 scale0)
             dy dx
             x41 (nth 0 p41)
             y41 (nth 1 p41)
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
         (princ "\ndraw axis")
         (setq p1 (polar p0 0 (+ d_d (* 5 scale0) (/ d_S 2.0)))
               p2 (polar p0 pi18 (+ d_d (* 55 scale0) (/ d_s 2.0)))
               p3 (polar p0 pi27 (+ (/ d_s 2.0) d_d (* 55 scale0)))
               p4 (polar p0 pi09 (+ (/ d_s 2.0) d_d (* 5 scale0)))
               rr (* 5 scale0)
               pc1 (polar p2 pi18 rr)  
               pc2 (polar p3 pi27 rr)
          ) ;setq end
          (command "line" p1 p2 "")
          (command "change" "l" "" "p" "lt" "axis" "")
          (command "line" p3 p4 "")
          (command "change" "l" "" "p" "lt" "axis" "")
          (command "circle" pc1 rr)          
          (command "circle" pc2 rr)
;draw dimension
         (setvar "dimdli" 0)
         (command "layer" "s" "pdim" "")
         (princ "\ndim1")
         (setq d2 (polar p0 pi18 (+ d_d (* 20 scale0) (/ d_S 2.0)))
               d1 (polar d2 pi27 H/2)
               d3 (Polar d2 pi09 H/2)
               dp (polar d1 pi18 (* 10 scale0))
         )
        (command "dim" "ver" d1 d2 dp "" "con" d3 "" "e")
        (Setq d1 (Polar p41 pi18 (* 30 scale0))
              d2 (polar d1 pi09 d_d)
              d3 (polar d2 pi09 (/ d_s 2.0))
              d4 (polar d3 pi09 (/ d_s 2.0))
              d5 (polar d4 pi09 d_d)
              dp (polar d1 pi18 (* 10 scale0))
        ) ;setq end
        (command "dim" "ver" d1 d2 dp "" "con" d3 "" "con" d4 ""
                 "con" d5 "" "e")
        (setq d1 dp
              d2 (Polar d3 pi18 (* 10 scale0))
              d3 (polar d5 pi18 (* 10 scale0))
              dp (polar d1 pi18 (* 10 scale0))
        ) ;setq end
        (command "dim" "ver" d1 d2 dp ""  "con" d3 "" "e")
         ;dim3
         (princ "\n dim3")
         (setq d2 (polar p41 0 (+ d_d (/ d_s 2.0)))
               d2 (polar d2 pi27 (* 20 scale0))
               d1 (polar d2 pi18 B/2)
               d3 (polar d2 0 B/2)
               dp (polar d1 pi27 (* 10 scale0))
          ) ;setq end
          (command "dim" "hor" d1 d2 dp "" "con" d3 "" "e")
          (Setq d1 (polar p41 pi27 (* 30 scale0))
                d2 (polar d1 0 d_d)
                d3 (polar d2 0 (/ d_s 2.0))
                d4 (polar d3 0 (/ d_s 2.0))
                d5 (polar d4 0 d_d)
                dp (polar d1 pi27 (* 10 scale0))
          ) ;setq end
          (command "dim" "hor" d1 d2 dp "" "con" d3 "" 
                   "con" d4 "" "con" d5 "" "e")
          (setq d1 dp
                d2 (polar d3 pi27 (* 10 scale))
                d3 (polar d5 pi27 (* 10 scale))
                dp (polar d1 pi27 (* 10 scale))
          ) ;setq end
          (command "dim" "hor" d1 d2 dp "" "con" d3 "" "e")
          (setq p01 (polar p0 pi09 (/ d_s 2.0))
                p01 (polar p01 pi18 (/ d_s 2.0))
                p02 (polar p01 0  d_S )
                p03 (polar p02 pi27 d_s)
                p04 (polar p01 pi27 d_s)
          ) ;setq end
          (draw_cir p01 d_d)
          (draw_cir p02 d_d)
          (draw_cir p03 d_d)
          (draw_cir p04 d_d)
) ;defun end

(defun pim_ct5(p0 j1 j2 )
       (command "layer" "s" "0" "")
       (setq x0 (nth 0 p0)
             y0 (nth 1 p0)
             pi09 (* pi 0.5)
             pi18 pi
             pi27 (* pi 1.5)
             B/2 (/ d_zb 2.0)
             H/2 (/ d_zh 2.0)
             D/2 (/ d_d 2.0)
             R (* 1.8 d_d)
       ) ;setq end    
       (setq p41 (polar p0 pi18 (+ d_d (/ d_s 2.0)))
             p42 (Polar p41 0 (+ (* d_d 2.0) d_s))
             p43 (polar p42 pi09 (+ (* 2.0 d_d) d_s))
             p44 (polar p43 pi18 (+ (* d_D 2.0) d_s))
       ) ;setq end
       (command "line" p41 p42 p43 p44 "c")
       (setq p0 (Polar p0 pi09 (+ d_d (/ d_s 2.0)))
             p11 (polar p0 pi27 H/2)   
             p11 (polar p11 pi18 B/2)
             p12 (polar p11 0 d_zb)
             p13 (polar p12 pi09 d_zh)
             p14 (polar p13 pi18 d_zb)
             p150 (polar p14 pi27 d_zh)
             p15 (intp 2 p41 p150 R)
       ) ;setq end      
       (command "line" p11 p12 p13 p14 p15 "")
; draw arc       
       (setq p1 (polar p41 0 R)
             p2 (polar p41 pi09 R)
       )
       (command "arc" "c" p41 p1 p2)
; draw reinforcement       
       (command "layer" "m" "rei" "c" "1" "" "")
       (setq scale0 (getvar "userr1")
             dx (* 2 scale0)
             dy dx
             x41 (nth 0 p41)
             y41 (nth 1 p41)
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
         (princ "\ndraw axis")
         (setq p1 (polar p0 0 (+ d_d (* 5 scale0) (/ d_S 2.0)))
               p2 (polar p0 pi18 (+ d_d (* 55 scale0) (/ d_s 2.0)))
               p3 (polar p0 pi27 (+ (/ d_s 2.0) d_d (* 55 scale0)))
               p4 (polar p0 pi09 (+ (/ d_s 2.0) d_d (* 5 scale0)))
               rr (* 5 scale0)
               pc1 (polar p2 pi18 rr)  
               pc2 (polar p3 pi27 rr)
          ) ;setq end
          (command "line" p1 p2 "")
          (command "change" "l" "" "p" "lt" "axis" "")
          (command "line" p3 p4 "")
          (command "change" "l" "" "p" "lt" "axis" "")
          (command "circle" pc1 rr)          
          (command "circle" pc2 rr)
;draw dimension
         (setvar "dimdli" 0)
         (command "layer" "s" "pdim" "")
         (princ "\ndim1")
         (setq d2 (polar p0 pi18 (+ d_d (* 20 scale0) (/ d_S 2.0)))
               d1 (polar d2 pi27 H/2)
               d3 (Polar d2 pi09 H/2)
               dp (polar d1 pi18 (* 10 scale0))
         )
        (command "dim" "ver" d1 d2 dp "" "con" d3 "" "e")
        (Setq d1 (Polar p41 pi18 (* 30 scale0))
              d2 (polar d1 pi09 d_d)
              d3 (polar d2 pi09 (/ d_s 2.0))
              d4 (polar d3 pi09 (/ d_s 2.0))
              d5 (polar d4 pi09 d_d)
              dp (polar d1 pi18 (* 10 scale0))
        ) ;setq end
        (command "dim" "ver" d1 d2 dp "" "con" d3 "" "con" d4 ""
                 "con" d5 "" "e")
        (setq d1 dp
              d2 (Polar d3 pi18 (* 10 scale0))
              d3 (polar d5 pi18 (* 10 scale0))
              dp (polar d1 pi18 (* 10 scale0))
        ) ;setq end
        (command "dim" "ver" d1 d2 dp ""  "con" d3 "" "e")
         ;dim3
         (princ "\n dim3")
         (setq d2 (polar p41 0 (+ d_d (/ d_s 2.0)))
               d2 (polar d2 pi27 (* 20 scale0))
               d1 (polar d2 pi18 B/2)
               d3 (polar d2 0 B/2)
               dp (polar d1 pi27 (* 10 scale0))
          ) ;setq end
          (command "dim" "hor" d1 d2 dp "" "con" d3 "" "e")
          (Setq d1 (polar p41 pi27 (* 30 scale0))
                d2 (polar d1 0 d_d)
                d3 (polar d2 0 (/ d_s 2.0))
                d4 (polar d3 0 (/ d_s 2.0))
                d5 (polar d4 0 d_d)
                dp (polar d1 pi27 (* 10 scale0))
          ) ;setq end
          (command "dim" "hor" d1 d2 dp "" "con" d3 "" 
                   "con" d4 "" "con" d5 "" "e")
          (setq d1 dp
                d2 (polar d3 pi27 (* 10 scale))
                d3 (polar d5 pi27 (* 10 scale))
                dp (polar d1 pi27 (* 10 scale))
          ) ;setq end
          (command "dim" "hor" d1 d2 dp "" "con" d3 "" "e")
          (setq p01 (polar p0 pi09 (/ d_s 2.0))
                p01 (polar p01 pi18 (/ d_s 2.0))
                p02 (polar p01 0  d_S )
                p03 (polar p02 pi27 d_s)
                p04 (polar p01 pi27 d_s)
                p05 (polar p01 0 (/ d_s 2.0))
                p05 (polar p05 pi27 (/ d_s 2.0))
          ) ;setq end
          (draw_cir p01 d_d)
          (draw_cir p02 d_d)
          (draw_cir p03 d_d)
          (draw_cir p04 d_d)
          (draw_cir p05 d_d)
) ;defun end
;
(defun pim_ct6(p0 j1 j2 )
       (command "layer" "s" "0" "")
       (Setq d_sx (* d_s 2.0)
             d_sy d_s
       ) ;setq end
       (setq x0 (nth 0 p0)
             y0 (nth 1 p0)
             pi09 (* pi 0.5)
             pi18 pi
             pi27 (* pi 1.5)
             B/2 (/ d_zb 2.0)
             H/2 (/ d_zh 2.0)
             D/2 (/ d_d 2.0)
             R (* 1.8 d_d)
       ) ;setq end    
       (setq p41 (polar p0 pi18 (+ d_d (/ d_sx 2.0)))
             p42 (Polar p41 0 (+ (* d_d 2.0) d_sx))
             p43 (polar p42 pi09 (+ (* 2.0 d_d) d_sy))
             p44 (polar p43 pi18 (+ (* d_D 2.0) d_sx))
       ) ;setq end
       (command "line" p41 p42 p43 p44 "c")
       (setq p0 (Polar p0 pi09 (+ d_d (/ d_s 2.0)))
             p11 (polar p0 pi27 H/2)   
             p11 (polar p11 pi18 B/2)
             p12 (polar p11 0 d_zb)
             p13 (polar p12 pi09 d_zh)
             p14 (polar p13 pi18 d_zb)
             p150 (polar p14 pi27 d_zh)
             p15 (intp 2 p41 p150 R)
       ) ;setq end      
       (command "line" p11 p12 p13 p14 p15 "")
; draw arc       
       (setq p1 (polar p41 0 R)
             p2 (polar p41 pi09 R)
       )
       (command "arc" "c" p41 p1 p2)
; draw reinforcement       
       (command "layer" "m" "rei" "c" "1" "" "")
       (setq scale0 (getvar "userr1")
             dx (* 2 scale0)
             dy dx
             x41 (nth 0 p41)
             y41 (nth 1 p41)
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
         (princ "\ndraw axis")
         (setq p1 (polar p0 0 (+ d_d (* 5 scale0) (/ d_S 2.0)))
               p2 (polar p0 pi18 (+ d_d (* 55 scale0) (/ d_s 2.0)))
               p3 (polar p0 pi27 (+ (/ d_s 2.0) d_d (* 55 scale0)))
               p4 (polar p0 pi09 (+ (/ d_s 2.0) d_d (* 5 scale0)))
               rr (* 5 scale0)
               pc1 (polar p2 pi18 rr)  
               pc2 (polar p3 pi27 rr)
          ) ;setq end
          (command "line" p1 p2 "")
          (command "change" "l" "" "p" "lt" "axis" "")
          (command "line" p3 p4 "")
          (command "change" "l" "" "p" "lt" "axis" "")
          (command "circle" pc1 rr)          
          (command "circle" pc2 rr)
;draw dimension
         (setvar "dimdli" 0)
         (command "layer" "s" "pdim" "")
         (princ "\ndim1")
         (setq d2 (polar p0 pi18 (+ d_d (* 20 scale0) (/ d_sx 2.0)))
               d1 (polar d2 pi27 H/2)
               d3 (Polar d2 pi09 H/2)
               dp (polar d1 pi18 (* 10 scale0))
         )
        (command "dim" "ver" d1 d2 dp "" "con" d3 "" "e")
        (Setq d1 (Polar p41 pi18 (* 30 scale0))
              d2 (polar d1 pi09 d_d)
              d3 (polar d2 pi09 (/ d_sy 2.0))
              d4 (polar d3 pi09 (/ d_sy 2.0))
              d5 (polar d4 pi09 d_d)
              dp (polar d1 pi18 (* 10 scale0))
        ) ;setq end
        (command "dim" "ver" d1 d2 dp "" "con" d3 "" "con" d4 ""
                 "con" d5 "" "e")
        (setq d1 dp
              d2 (Polar d3 pi18 (* 10 scale0))
              d3 (polar d5 pi18 (* 10 scale0))
              dp (polar d1 pi18 (* 10 scale0))
        ) ;setq end
        (command "dim" "ver" d1 d2 dp ""  "con" d3 "" "e")
         ;dim3
         (princ "\n dim3")
         (setq d2 (polar p41 0 (+ d_d (/ d_sx 2.0)))
               d2 (polar d2 pi27 (* 20 scale0))
               d1 (polar d2 pi18 B/2)
               d3 (polar d2 0 B/2)
               dp (polar d1 pi27 (* 10 scale0))
          ) ;setq end
          (command "dim" "hor" d1 d2 dp "" "con" d3 "" "e")
          (Setq d1 (polar p41 pi27 (* 30 scale0))
                d2 (polar d1 0 d_d)
                d3 (polar d2 0 (/ d_sx 2.0))
                d4 (polar d3 0 (/ d_sx  2.0))
                d5 (polar d4 0 d_d)
                dp (polar d1 pi27 (* 10 scale0))
          ) ;setq end
          (command "dim" "hor" d1 d2 dp "" "con" d3 "" 
                   "con" d4 "" "con" d5 "" "e")
          (setq d1 dp
                d2 (polar d3 pi27 (* 10 scale))
                d3 (polar d5 pi27 (* 10 scale))
                dp (polar d1 pi27 (* 10 scale))
          ) ;setq end
          (command "dim" "hor" d1 d2 dp "" "con" d3 "" "e")
          (setq p01 (polar p0 pi09 (/ d_s 2.0))
                p01 (polar p01 pi18 (/ d_sx 2.0))
                p02 (polar p01 0  d_Sx )
                p03 (polar p02 pi27 d_s)
                p04 (polar p01 pi27 d_s)
                p05 (polar p01 0 (/ d_sx 2.0))
                p06 (polar p04 0 (/ d_sx 2.0))
          ) ;setq end
          (draw_cir p01 d_d)
          (draw_cir p02 d_d)
          (draw_cir p03 d_d)
          (draw_cir p04 d_d)
          (draw_cir p05 d_d)
          (draw_cir p06 d_d)
) ;defun end
;
        (defun fix1(x / d1 d txt delta)
          (setq d1 (fix (/ x 5))
                delta (- x (* d1 5))
                d1 (if (>= delta 2.5) (1+ d1) d1)
                x (* d1 5)
          ) ;setq end
          (setq txt (itoa x))
        )  ;defun end
;
(defun draw_cir(p_cir d_cir / p1 p2 r_cir)
       (setvar "cmdecho" 0)
       (Setq 
             r_cir (/ d_cir 2.0)
             p1 (polar p_cir 0 (+ r_cir (* 2.0 scale)))
             p2 (polar p_cir pi18 (+ r_cir (* 2.0 scale)))
       ) ;setq end
       (command "layer" "s" "pdim" "")
       (command "layer" "s" "0" "")
       (command "line" p1 p2 "")
       (Setq 
             p1 (polar p_cir pi09 (+ r_cir (* 2.0 scale)))
             p2 (polar p_cir pi27 (+ r_cir (* 2.0 scale)))
       ) ;setq end
       (command "line" p1 p2 "")
       (command "circle" p_cir r_cir)
       (command "change" "l" "" "p" "lt" "dashed2" "")
       (command "layer" "s" "0" "")
) ;defun end
;
;
(defun pim_ct3(p0 j1 j2 / x0 y0 B/2 H/2 D/2 s1)
       (command "layer" "s" "0" "")
       (setq x0 (nth 0 p0)
             y0 (nth 1 p0)
             pi09 (* pi 0.5)
             pi18 pi
             pi27 (* pi 1.5)
             B/2 (/ d_zb 2.0)
             H/2 (/ d_zh 2.0)
             D/2 (/ d_d 2.0)
             s1 d_s
             pi06 (/ pi 3.0)
             pi03 (/ pi 6.0)
             s2 (+ s1 (/ (* 3 d_d) (sin pi06)))
             sh1 (* s1 (sin pi06))
             sh2 (* s2 (sin pi06))
             p01 (polar p0 pi18 (/ s2 2.0))
             pp0 (polar p0 pi09 d_d)
             p1 (Polar pp0 pi18 (/ s1 2.0))
             p0 (polar p0 pi09 (/ sh2 3.0))
             p2 (polar p1 0 s1)
             p3 (polar p1 pi06 s1)
             p02 (polar p01 0 s2)
             p03 (polar p01 pi06 s2)
             sh2/3 (/ (* sh2 2.0) 3.0)
             sh1/3 (/ (* sh1 2.0) 3.0)
             dist (- sh2/3 sh1/3 d_d)
             dist (/ dist (cos pi03))
             pp1 (polar p01 0 dist)
             pp2 (polar p01 pi06 dist)
             pp3 (polar p03 (* -2.0 pi06) dist)
             pp4 (polar p03 (* -1.0 pi06) dist)
             pp5 (polar p02 (* 2.0 pi06) dist)
             pp6 (polar p02 pi18 dist)
       ) ;setq end    
       (command "line" pp1 pp2 pp3 pp4 pp5 pp6 "c")
       (draw_cir p1 d_d)
       (draw_cir p2 d_d)
       (draw_cir p3 d_d)
       (setq p11 (polar p0 pi27 H/2)   
             p11 (polar p11 pi18 B/2)
             p12 (polar p11 0 d_zb)
             p13 (polar p12 pi09 d_zh)
             p14 (polar p13 pi18 d_zb)
       ) ;setq end      
       (command "line" p11 p12 p13 p14 "c")
; draw arc       
       (setq delta (* 0.4 d_d)
             R (+ delta (* 0.9 D/2))
             p41 (polar p0 (* 5.0 pi03) (+ delta (/ sh2 3.0)))
             alfa (sqrt (- (* R R) (* delta delta)))
             alfa (/ alfa delta)
             alfa (atan alfa)
             alfa1 (- alfa pi03)
             alfa2 (* -1.0 (+ alfa pi03))
             p1 (polar p41 alfa1 R)
             p2 (polar p41 alfa2 R)
       )
       (command "arc" "c" p41 p2 p1)
; draw reinforcement       
       (command "layer" "m" "rei" "c" "1" "" "")
       (setq scale0 (getvar "userr1")
             dx (* 2 scale0)
             dy dx
             dist (- (distance p1 p2) dx)
             n (fix (/ dist (* 5 scale0)))
             p2 (polar p2 (* 0.25 pi) dx)
             index 0
        ) ;setq end     
        (repeat (1+ n)
            (setq dx (* index (* 5 scale0))
                  dy dx
                  index (1+ index)
                  po1 (polar p2 pi06 dx)
                  po2 (intp 1 p41 po1 R) 
                  po3 (intp 3 p41 po1 R)
            ) ;setq end
            (command "line" po1 po2 "")
            (command "line" po1 po3 "")
        ) ;repeat end
;draw rei num1
        (command "layer" "s" "0" "")
        (setq pc (polar p41 pi18 (* 25 scale))
              pc (polar pc pi09 (* 10 scale))
              pr (* 4 scale0)
              htext (* 4 scale0)
              pj (polar pc 0 (* scale0 5))
              pj (polar pj pi27 (* scale0 2))
         ) ;setq end
         (command "circle" pc pr)
         (command "text" "j" "m" pc htext 0 "2")
         (command "text" pj htext 0 j2) 
;draw rei num2
;
        (setq dy (* 4.0 scale0)
              n (fix (/ d_d dy))
              ss (ssadd)
              index 0
              p00 (Polar p0 pi27 (- (/ sh2 3.0) D/2))
        ) ;setq end
        (command "layer" "s" "rei" "")
        (repeat n
            (setq p (polar p00 pi09 (* index dy))
                  index (1+ index)
                  s (/ s2 2.0)
            ) ;sete end
          (setq p_1 (polar p pi18 s)
                p_int (inters pp1 pp2 p p_1)
          ) ;setq end
          (if (not p_int )
              (Setq p_int (inters pp2 pp3 p p_1))
           ) ;if end
          (setq p1 (polar p_int 0 (* 1.0 scale)))
          (setq p_1 (polar p 0 s)
                p_int (inters pp6 pp5 p p_1)
          ) ;setq end
          (if (not p_int )
              (Setq p_int (inters pp5 pp4 p p_1))
           ) ;if end
          (setq p2 (polar p_int pi18 (* 1.0 scale)))
            (command "line" p1 p2 "")
            (setq ename (entlast)
                  ss (ssadd ename ss)
            ) ;setq end
         ) ;repeat end
         (setq p1 (polar p 0 (* 5 scale))
               p1 (polar p pi09 (* 2.0 scale))
               p2 (polar p1 pi27 (+ (* 1.5 d_d) (* 10 scale)))
               p3 (polar p2 0 (* 30 scale))
               pc (polar p3 0 (* 4 scale))
               pj (polar p2 0 (* 5 scale))
               pj (polar pj pi09 (* 1.0 scale))
         ) ;setq end
         (command "layer" "s" "0" "")
         (command "line" p1 p2 "")
            (setq ename (entlast)
                  ss (ssadd ename ss)
            ) ;setq end
         (command "line" p2 p3 "")
            (setq ename (entlast)
                  ss (ssadd ename ss)
            ) ;setq end
         (command "circle" pc pr)
            (setq ename (entlast)
                  ss (ssadd ename ss)
            ) ;setq end
         (command "text" "j" "m" pc htext 0 "1")
            (setq ename (entlast)
                  ss (ssadd ename ss)
            ) ;setq end
         (command "text" pj htext 0 j1) 
            (setq ename (entlast)
                  ss (ssadd ename ss)
            ) ;setq end
         (command "array" ss "" "p" p0 3 "" "y")
;draw axis
         (princ "\ndraw axis")
         (setq p1 (polar p0 0 (+ d_d (* 5 scale0)))
               p2 (polar p0 pi18 (+ d_d (* 55 scale0)))
               p3 (polar p0 pi27 (+ (/ d_s 2.0) d_d (* 55 scale0)))
               p4 (polar p0 pi09 (+ (/ d_s 2.0) d_d (* 5 scale0)))
               rr (* 5 scale0)
               pc1 (polar p2 pi18 rr)  
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
         (princ "\ndimlfac:") (princ (/ 1.0 factor))
         (setvar "dimlfac" (/ 1.0 factor))
         (setvar "dimdli" 0)
         ;(princ "\ndim1")
         (setq d2 (polar p0 pi18 (+ (/ s2 2.0) (* 20 scale0)))
               d1 (polar d2 pi27 H/2)
               d3 (Polar d2 pi09 H/2)
               dp (polar d1 pi18 (* 10 scale0))
         ) ;setq end
        (command "dim" "ver" d1 d2 dp "" "con" d3 "" "e")
;        
        (setq dist (- sh2/3 sh1/3 d_d)
              dist1 dist
              dist2 (- (/ sh2 3.0) dist1)
              dist3 sh1/3
              dist4 d_d
              d1 (polar p01 pi18 (* 30 scale))
              d2 (polar d1 pi09 dist1)
              d3 (polar d2 pi09 dist2)
              d4 (polar d3 pi09 dist3)
              d5 (polar d4 pi09 dist4)
              x (/ dist1 factor)
              txt1 (fix1 x)
              x (/ dist2 factor)
              txt2 (fix1 x)
              x (/ dist3 factor)
              txt3 (fix1 x)
              x (/ dist4 factor)
              txt4 (fix1 x)
              dp (polar d1 pi18 (* 10 scale0))
        ) ;setq end
        (command "dim" "ver" d1 d2 dp txt1 "con" d3 txt2 "con" d4 txt3
                 "con" d5 txt4 "e")
        (setq d1 dp
              d2 (Polar d3 pi18 (* 10 scale0))
              d3 (polar d5 pi18 (* 10 scale0))
              x (/ (+ dist1 dist2) factor)
              txt1 (fix1 x)
              x (/ (+ dist3 dist4) factor)
              txt2 (fix1 x)
              dp (polar d1 pi18 (* 10 scale0))
        ) ;setq end
        (command "dim" "ver" d1 d2 dp txt1 "con" d3 txt2 "e")
         ;dim3
         (princ "\n dim3")
         (setq d2 (polar p0 pi27 (/ sh2 3.0))
               d2 (polar d2 pi27 (* 20 scale0))
               d1 (polar d2 pi18 B/2)
               d3 (polar d2 0 B/2)
               dp (polar d1 pi27 (* 10 scale0))
          ) ;setq end
          (command "dim" "hor" d1 d2 dp "" "con" d3 "" "e")
          (setq dist (/ dist (cos pi03))
                dist1 (/ dist 2.0)
                dist2 (- (/ s2 2.0) (/ s1 2.0) dist)
                dist3 (/ s1 2.0)
                x (/ dist1 factor)
                txt1 (fix1 x)
                x (/ dist2 factor)
                txt2 (fix1 x)
                x (/ dist3 factor)
                txt3 (fix1 x)
                d1 (polar p01 0 dist1)
                d1 (polar d1 pi27 (* 30 scale))
                d2 (polar d1 0 dist1)
                d3 (polar d2 0 dist2)
                d4 (polar d3 0 dist3)
                d5 (polar d4 0 dist3)
                d6 (polar d5 0 dist2)
                d7 (polar d6 0 dist1)
                dp (polar d1 pi27 (* 10 scale0))
                d8 dp
                d9 (polar d4 pi27 (* 10 scale0))
                d10 (polar d7 pi27 (* 10 scale0))
                dp1 (polar d9 pi27 (* 10 scale0))
                x (/ (+ (atoi txt1) (atoi txt2) (atoi txt3)) factor)
                txt4 (fix1 x)
          ) ;setq end
          (command "dim" "hor" d1 d2 dp txt1 "con" d3 txt2
                   "con" d4 txt3 "con" d5 txt3 "con" d6 txt2
                   "con" d7 txt1 "e")
          (command "dim" "hor" d8 d9 dp1 txt4 "con" d10 txt4 "e")
          (command "layer" "s" "0" "")
) ;defun end
;      

