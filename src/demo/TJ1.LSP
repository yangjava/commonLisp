(VMON)
(setvar "cmdecho" 0)
(command "layer" "m" "rei" "c" "1" "" "s" "0" "")
(command "layer" "m" "pdim" "c" "green" "" "s" "0" "")
;
(setq scale (getvar "userr1")
      scale1 scale
      factor 1
) ;setq  end
;
(defun draw_ql(p_ql / p1 p2 p3 p4 pr1 pr2 pr3 pr4 r1 r2)
 (setq p1 p_ql
       p2 (polar p1 0 tj_qh)
       p3 (polar p1 pi27 tj_ql)
       p4 (polar p2 pi27 tj_ql)
 ) ;setq end
 (command "layer" "s" "0" "")
 (command "line" p1 p2 "")
 (command "line" p3 p4 "")
 (Setq pr1 (polar p1 0 scale)
       pr1 (polar pr1 pi27 scale)
       pr2 (polar pr1 0 (- tj_qh (* 2.0 scale)))
       pr3 (polar pr1 pi27 (- tj_ql (* 2.0 scale)))
       pr4 (polar pr2 pi27 (- tj_ql (* 2.0 scale)))
 ) ;setq end
 (command "layer" "s" "rei" "")
 (command "line" pr1 pr2 pr4 pr3 "c")
 (Setq pr1 (polar pr1 0 scale)
       pr1 (polar pr1 pi27 scale)
       pr2 (polar pr1 0 (- tj_qh (* 4.0 scale)))
       pr3 (polar pr1 pi27 (- tj_ql (* 4.0 scale)))
       pr4 (polar pr2 pi27 (- tj_ql (* 4.0 scale)))
       r1 0 r2 scale
 ) ;setq end
 (command "layer" "s" "0" "")
 (command "donut" r1 r2 pr1 pr2 pr3 pr4 "")
) ;defun end
;
(defun tj_dash(p_dash b / p_p1 p_p2 p_p3 p_p4 p_p5 p_p6)
 (setq p_p1 (polar p_dash pi18 (+ (/ b 2) (* 2 scale)))
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
(defun C:DRAW_TJ( )
 (setvar "blipmode" 0)
 (setvar "cmdecho" 0)
 (setq oer *error* *error* clmerr)
 (setq scale0 (getvar "userr1"))
 (setvar "dimdli" 0)
 (command "zoom" "a")
 (setvar "dimlfac" (/ 1.0 factor))
   ;
     (setq  l1 (+ (* scale0 20) tj_b)
            l2 (+ (* scale0 70) tj_h)
      ) ;setq end
      (princ "\n条基插入点:")
      (command "insert" "border" "x" l1 "y" l2 "r" 0 pause)
      (setq p0 (getvar "lastpoint"))
      (command "erase" "l" "")
      (setq p2 (polar p0 0 (* 15 scale))
            p1 (polar p2 0 (/ tj_b 2.0))            
            p3 (polar p2 pi09 tj_h)
            p4 (polar p1 pi09 tj_h)
       ) ;setq end
       (command "layer" "s" "0" "")     
       (command "line" p1 p2 p3 p4 "")
       (if (> (- (nth 0 l_tb) 0) 0.001)
           (progn
              (setq sum_th 0 sum_tb 0 index 0)
              (repeat n_tb
                (setq sum_tb (+ sum_tb (nth index l_tb))  
                      sum_th (+ sum_th (nth index l_th))
                      index (1+ index)
                ) ;setq end
              ) ;repeat end
;              (princ "\nrepeat 0 end")
              (setq p0_tj (polar p4 pi09 sum_th)
                    dh0_tj (polar p0_tj pi09 (* 5 scale))
                    p1_tj (polar p0_tj pi18 (/ tj_qh 2.0))
                    dh1_tj (polar dh0_tj pi18 (/ tj_qh 2.0))
                    dhp    (polar dh1_tj pi09 (* 10 scale))
                    dv1_tj (polar p0_tj pi18 (+ (* 5 scale) (/ tj_b 2.0)))
                    dvp    (polar dv1_tj pi18 (* 10 scale))
                    index 0
              ) ;setq end
;              (princ "\nrepeat 1 end")
              (command "layer" "s" "pdim" "")
              (command "dim1" "hor" dh0_tj dh1_tj dhp "")
              (command "layer" "s" "0" "")
              (repeat n_tb
                  (setq n_b (nth index l_tb)
                        n_h (nth index l_th)
                        index (1+ index)
                        p2_tj (polar p1_tj pi18 n_b)
                        p3_tj (polar p2_tj pi27 n_h)
                        dh2_tj (polar dh1_tj pi18 n_b)
                        dv2_tj (polar dv1_tj pi27 n_h)
                   ) ;setq end
                   (command "layer" "s" "0" "")
                   (command "line" p1_tj p2_tj p3_tj "")
                   (command "layer" "s" "pdim" "")
                   (command "dim1" "hor" dh1_tj dh2_tj dhp "")
                   (command "dim1" "ver" dv1_tj dv2_tj dvp "")
                   (setq p1_tj  p3_tj
                         dh1_tj dh2_tj
                         dv1_tj dv2_tj
                    ) ;setq end
             ) ;repeat end
           ) ;progn end  
             (setq p0_tj p4)
       ) ;if end
       (setq p1_tj (polar p0_tj pi18 (/ tj_qh 2.0))
             p2_tj (polar p1_tj pi09 (* 50 scale))
             p3_tj (polar p0_tj pi09 (* 50 scale))
             p_ql (polar p2_tj pi27 (+ (* 5 scale) (* 60 factor)))
             p_00 (polar p3_tj pi27 (* 5 scale))
             p_00 (polar p_00 pi18 (+ (/ tj_b 2.0) (* 20 scale)))
             lx (/ scale 100.0) ly lx
       ) ;setq end
       (command "layer" "s" "0" "")
       (command "line" p1_tj p2_tj "")
       (command "layer" "s" "pdim" "")
       (command "insert" "elul" p_00 lx ly 0 "")
       (command "layer" "s" "0" "")
       (command "mirror" "c" p2 p3_tj "" p4 p3_tj "n")
       (if (/= tj_ql 0) 
          (progn
            (draw_ql p_ql)
            (setq d1_ql (polar p_00 0 (* 15 scale))
                  d2_ql (polar d1_ql pi27 (* 60 factor))
                  d3_ql (polar d2_ql pi27 tj_ql)
                  dp_ql (polar d3_ql pi18 (* 10 scale))
            ) ;setq end
            (command "layer" "s" "pdim" "")
            (command "dim" "ver" d1_ql d2_ql dp_ql "" "con" d3_ql "" "e")
            (command "layer" "s" "0" "")
          ) ;progn end
       ) ;if end
       (tj_dash p3_tj tj_qh)
       (setq p_bg (polar p2 pi18 (* 20 scale))
             pv1  (polar p_bg 0 (* 15 scale))
             pv2  (polar pv1 pi09 tj_h)
             dv   (polar pv2 pi18 (* 10 scale))
             ph1  (polar p2 pi27 (* 10 scale))
             ph2  (polar ph1 0 (/ tj_b 2.0))
             ph3  (polar ph2 0 (/ tj_b 2.0))
             dh   (polar ph1 pi27 (* 10 scale))
             p1_ax (polar p1 pi27 (* 10 scale))
             p2_ax (polar p3_tj pi09 (* 3 scale))
       ) ;setq end
       (command "layer" "s" "pdim" "")
       (command "insert" "elul" p_bg lx ly 0 tj_bg)
       (command "dim1" "ver" pv1 pv2 dv "")
       (command "dim" "hor" ph1 ph2 dh "" "con" ph3 "" "e")
       (command "line" p1_ax p2_ax "")
       (command "change" "l" "" "p" "lt" "axis" "")
       (command "zoom" "p")
       (setvar "cmdecho" 1)
       (setvar "dimlfac" 1)
) ;defun end
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
(defun C:TJCS()
 (setq tj_b (getreal "\n条基宽:(mm)")
       tj_b (* tj_b factor)
       tj_h (getreal "\n条基高:(mm)")
       tj_h (* tj_h factor)
       tj_qh (getreal "\n墙厚:<240>")
       tj_qh (if tj_qh tj_qh 240)
       tj_qh (* tj_qh factor)
       tj_ql (getreal "\n圈梁高:<180>")
       tj_ql (if tj_ql tj_ql 180)
       tj_ql (* tj_ql factor)
  ) ;setq end       
  (princ "\n从墙角放出各级台阶依次宽<0>:")
  (load "tab.lsp")
  (setq scale factor)
  (setq l_tb (readlh))
  (if (> (- (nth 0 l_tb) 0) 0.0001)
    (progn  
      (princ "\n从墙角放出各级台阶从上到下依次高:")
      (setq l_th (readlh))
    ) ;progn end
    (setq l_th '(0) )
  ) ;if end
  (setq n_tb (length l_tb)
        b_th (length l_th)
        tj_bg (getstring "\n条基底标高:(m)")
  ) ;setq end       
  (setq scale (getvar "userr1"))
) ;defun CTCS end
;        

