(VMON)
(command "layer" "m" "rei" "c" "1" "" "s" "0" "")
(command "layer" "m" "pdim" "c" "green" "" "s" "0" "")
;
(defun zlm_dash(p_dash)
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
; defun zong xin xi shu ru
;----------
(DEfun C:ZZXX( / x y txt l-zxx p0 p1 x0 y dy ny rea htext)
 (setvar "cmdecho" 0)
 (setq oer *error* *error* clmerr)
 (setq scale (getvar "userr1")
       b 1000
       blh 100
       j-wz "1"
       j-cc "0"
       txt-ln "ln"
       txt-ll "l"
       txt-cg "结构板面"
       jcbg -2.30
       txt-jc "基础顶标高"
       txt-gj "箍筋加密区"
 ) ;setq end
 (command "zoom" "a")
 (setq temp (getstring "\n中柱或角柱(1/0):<1>")
       j-wz (if (/= temp "") temp "1")
       temp (getreal "\n楼面梁高(mm):<1000>")
       b (if temp temp 1000)
       temp (getint "\n柱高方向局部比例:<100>") 
       blh (if temp temp 100)
       temp (getstring "\n是否标实际尺寸(1/0):<0>")
       j-cc (if (/= temp "") temp 0)
 ) ;setq end
  (if (= j-cc "1")
    (setq ln (getreal "\n柱箍筋加密区长度(mm):")
          txt-ln (rtos ln)
          jcbg (getreal "\基础顶标高(m):")
          txt-jc (rtos jcbg)
    ) ;setq end      
  )   ;if end
(command "zoom" "p")
(setvar "cmdecho" 1)
) ;defun end
;
;------------
;---------
;
(defun C:ZKBZ( / temp)
 (setvar "cmdecho" 0)
 (setq oer *error* *error* clmerr)
 (setq temp T)
 (while temp
  (princ "\n输入各层高度,格式如: 5,4,3(3.6),4\n>(米):")
  (Load "tab.lsp")
  (setq scale 1000)
  (setq l-l (readlh))
  (princ "\n输入各层柱截面高,格式如: .7,4(.6),.5\n>(米):")
  (setq l-h (readlh)    
        n-l (length l-l)
        n-h (length l-h)
  ) ;setq end
  (setq scale (getvar "userr1"))
  (if (/= n-l n-h) (princ "\n输入错误:柱高个数与层数不符") (setq temp nil))
 ) ;while end
 (setvar "cmdecho" 1)
) ;defun end  

;
(defun draw_zhu(l h p0 txt-cg j-dc)
 (command "layer" "s" "0" "")
 ;draw right edge line
 (setq scale (getvar "userr1")
       hh (* h (/ scale blh))
       d1 (if (= j-wz "1") (* 10 scale) 0)
       d2 (* 10 scale)
       p1 (polar p0 pi18 d1)
       p2 (polar p1 pi09 b)
       p1 (if (= j-wz "1") p1 p2)
       p3 (polar p2 0 d1)
       p4 (polar p3 pi09 (- l b))
       p5 (polar p0 0 hh)
       p6 (polar p5 0 (* 10 scale))
       p7 (polar p6 pi09 b)
       p8 (polar p7 pi18 (* 10 scale))
       p9 (polar p8 pi09 (- l b))
) ;setq end
 (command "line" p0 p1 "" )
 (command "line" p2 p3 p4 "") 
 (command "line" p5 p6 "") 
 (command "line" p7 p8 p9 "")
 ;draw up edge line
 (setq pr1 (polar p0 0 (* 1.0 scale))
       pr2 (polar pr1 pi09 l)
       pr3 (polar pr1 0 (- hh (* 2.0 scale)))
       pr4 (polar pr3 pi09 l)
 ) ;setq end
 (command "layer" "s" "rei" "")
 (command "line" pr1 pr2 "" )
 (setq dy (- hh (* 2 scale))
       dy (/ dy 2.0)
  ) ;setq end
  (command "array" "l" "" "r" 1 3 dy)
 ;draw dimension
 (setq txt-ll (if (= j-cc 0) "l" (rtos l))
 ) ;setq end
 (setq ln (/ (- l b) 6.0)
       d1 (polar p0 pi18 (* 20 scale)) 
       d1 (polar d1 pi09 b)
       d2 (polar d1 pi09 ln)
       d3 (polar d1 pi09 (- l b))
       d4 (polar d3 pi27 ln)
       dp1 (polar d1 pi18 (* 10 scale))
       dln (if (= j-dc 1) b (+ b ln))
       d5 (polar d3 pi09 dln)
       d5 (polar d5 pi18 (* 10 scale))
       d6 (polar d4 pi18 (* 10 scale))
       dp2 (polar d5 pi18 (* 10 scale))
) ;setq end      
 (command "layer" "s" "pdim" "")
 (setvar "dimdli" 0)
 (command "dim" "ver" d1 d2 dp1 txt-ln "" d3 d4 dp1 txt-ln "" 
          d5 d6 dp2 txt-gj "e")
 (setq p_bg (polar d1 pi18 (* 30 scale))
       sc_x (/ scale 100.0)
       sc_y sc_x
 ) ;setq end
 (command "insert" "elul" p_bg sc_x sc_y 0 txt-cg)
 (command "layer" "s" "0" "")
);defun end
;------
;
(defun draw_jc(jcbg txt-jc p0 h j-dc)
 (setq scale (getvar "userr1")
       hh (* h (/ scale blh))
 ) ;setq end
 (setq p1 (polar p0 pi18 (* 15 scale))
       p2 (polar p1 pi09 (* 15 scale))
       p3 (polar p2    0 (* 15 scale))
       p4 (polar p0 0 hh)
       p5 (polar p4 0 (* 15 scale))
       p6 (polar p5 pi09 (* 15 scale))
       p7 (polar p6 pi18 (* 15 scale))
 ) ;setq end
 (command "layer" "s" "0" "")
 (command "line" p0 p1 p2 p3 "")
 (command "line" p4 p5 p6 p7 "")
 (setq p1 p3
       p3 p7
       bg1 (* (abs jcbg) 1000)
       p2 (polar p1 pi09 (- l b))
       p4 (polar p2 0 hh)
 ) ;setq end
 (command "layer" "s" "0" "")
 (command "line" p1 p2 "")
 (command "line" p3 p4 "")
 (Setq pr1 (polar p0 0 (* 1.0 scale))
       pr2 (polar pr1 pi09 (+ l (* 15 scale) (* -1 b)))
 ) ;setq end
 (command "layer" "s" "rei" "")
 (command "line" pr1 pr2 "")
 (setq dy (- hh (* 2 scale))
       dy (/ dy 2.0)
  ) ;setq end
  (command "array" "l" "" "r" 1 3 dy)
 (command "layer" "s" "pdim" "")
;draw dimension        
 (setq ln (/ (- l b) 6.0)
       p1 (polar p0 pi09 (* 15 scale))
       temp (polar p1 pi09 bg1)
       d1 (polar temp pi18 (* 20 scale)) 
       d2 (polar d1 pi09 ln)
       l (- l bg1)
       d3 (polar d1 pi09 (- l b))
       d4 (polar d3 pi27 ln)
       dp1 (polar d1 pi18 (* 10 scale))
       dln (if (= j-dc 1) b (+ b ln))
       d5 (polar d3 pi09 dln)
       d5 (polar d5 pi18 (* 10 scale))
       d6 (polar d4 pi18 (* 10 scale))
       dp2 (polar d5 pi18 (* 10 scale))
) ;setq end      
 (setvar "dimdli" 0)
 (command "dim" "ver" d1 d2 dp1 txt-ln "" d3 d4 dp1 txt-ln "" 
          d5 d6 dp2 txt-gj "e")
 (setq p_bg (polar d1 pi18 (* 30 scale))
       sc_x (/ scale 100.0)
       sc_y sc_x
 ) ;setq end
 (command "insert" "elul" p_bg sc_x sc_y 0 "%%1280.000")
 (setq d1 (polar p1 pi18 (* 20 scale))
       p_bg (polar d1 pi18 (* 30 scale))
       d1 (polar p1 pi18 (* 20 scale))
       d2 (polar d1 pi09 (+ bg1 ln))
       dp (polar d1 pi18 (* 20 scale))
  ) ;setq end
 (command "dim" "ver" d1 d2 dp txt-gj "e")
 (command "insert" "elul" p_bg sc_x sc_y 0 txt-jc)
 (setq r1 (polar p1 0 (* 1.0 scale))
       r2 (polar r1 (* 0.25 pi) (* 2 scale))
       dr (/ (- hh (* 2 scale)) 2.0)
       r3 (polar r1 0 dr)
       r4 (polar r3 (* 0.25 pi) (* 2 scale))
       r5 (polar r3 0 dr)
       r6 (polar r5 (* 0.75 pi) (* 2 scale))
   ) ;setq end
   (command "layer" "s" "rei" "")
   (command "line" r1 r2 "")
   (command "line" r3 r4 "")
   (command "line" r5 r6 "")
 (setq r1 (polar r1 pi09 (* 15 scale))
       r2 (polar r1 (* -0.25 pi) (* 2 scale))
       dr (/ (- hh (* 2 scale)) 2.0)
       r3 (polar r1 0 dr)
       r4 (polar r3 (* -0.25 pi) (* 2 scale))
       r5 (polar r3 0 dr)
       r6 (polar r5 (* -0.75 pi) (* 2 scale))
   ) ;setq end
   (command "line" r1 r2 "")
   (command "line" r3 r4 "")
   (command "line" r5 r6 "")
   (command "layer" "s" "0" "")    
   (setq r1 (polar r2 pi09 (* 15 scale))
         r3 (polar r4 pi09 (* 15 scale))
         r5 (polar r6 pi09 (* 15 scale))
   ) ;setq end
   (command "line" r1 r2 "")
   (command "line" r3 r4 "")
   (command "line" r5 r6 "")
   (Setq r2 (polar r1 0 (+ hh (* 20 scale)))
         ptxt1 (polar r1 0 (+ hh (* 5 scale)))
         ptxt1 (polar ptxt1 pi09 (* 1.0 scale))
         ptxt2 (polar ptxt1 pi27 (* 6.0 scale))
         htext (* 4.0 scale)
         txt1 "基础插筋"
         txt2 "规格同柱主筋"
   ) ;setq end
   (command "line" r1 r2 "")
   (command "text" ptxt1 htext 0 txt1)
   (command "text" ptxt2 htext 0 txt2)
);defun end
;------
;---------
;(setq p0 (getpoint "\np0:"))
;
(defun draw_zt(p0 txt-cg)
(setq scale (getvar "userr1")
       d (if (= j-wz "1") (* 10 scale) 0)
       hh (* h (/ scale blh))
       p1 (polar p0 pi09 b)
       p2 (polar p1 0 (+ hh (* 10 scale)))
       p3 (polar p0 0 hh)
       p4 (polar p3 0 (* 10 scale))
       p5 (polar p0 pi18 d)
       p6 (polar p5 pi09 b)
       p5 (if (= j-wz "1") p5 p1)
 ) ;setq end
 (command "layer" "s" "0" "")
 (command "line" p0 p5 "")
 (command "line" p6 p1 p2 "")
 (command "line" p3 p4 "")
 (setq pr1 (polar p0 0 (* 1.0 scale))
       d_pr1 (if (= j-wz "0") (* b 0.9) (* b 0.5))
       d_pr2 (if (= j-wz "0") (+ hh (* 3 scale)) (* 5 scale))
       ang_pr (if (= j-wz "0") 0 pi18)
       pr2 (polar pr1 pi09 d_pr1) 
       pr3 (polar pr2 ang_pr d_pr2) 
 );setq end      
 (command "layer" "s" "rei" "")
 (command "line" pr1 pr2 pr3 "")
 (Setq pr1 (polar p0 0 (/ hh 2.0))
       pr2 (polar pr1 pi09 (* 0.7 b))
       pr3 (polar pr2 0 (+ (/ hh 2.0) (* 4 scale)))
 ) ;setq end
 (command "line" pr1 pr2 pr3 "")
 (setq pr1 (polar p0 0 (- hh (* 1.0 scale)))
       pr2 (polar pr1 pi09 (* 0.5 b))
       pr3 (polar pr2 0 (* 4 scale))
 ) ;setq end
 (command  "line" pr1 pr2 pr3 "")
 (command "layer" "s" "0" "")
 (setq p_bg (polar p1 pi18 (* 50 scale))
       sc_x (/ scale 100.0)
       sc_y sc_x
 ) ;setq end
 (command "layer" "s" "pdim" "")
 (command "insert" "elul" p_bg sc_x sc_y 0 txt-cg)
 (command "layer" "s" "0" "")
) ;defun end
;
;
(defun C:HZ()
 (setvar "cmdecho" 0)
 (setq oer *error* *error* clmerr)
 (setq n-len (length l-l)
       add 0 
       scale (getvar "userr1")
       m 0 
 )
 (repeat n-len
   (setq add (+ add (abs (nth m l-l)))
         m (1+ m)
   );setq end
 ) ;repeat end
 (setq y (+ (nth 0 l-h) (* 70 scale)))
 (princ "\n柱插入点:")
 (command "insert" "border" "x" y "y" add "r" 0 pause)
 (setq p0 (getvar "lastpoint"))
 (command "erase" "l" "")
 (setq l (nth 0 l-l) h (nth 0 l-h) ll l)
 (Setq p0 (polar p0 0 (* 40 scale))
       j-dc (if (= n-len 1) 1 0)
       txt-jc (if (= j-cc "1") (rtos jcbg 2 2) txt-jc)
 ) ;setq end
 (draw_jc jcbg txt-jc p0 h j-dc)
(if (> n-len 1) 
 (setq dh (* (- (nth 1 l-h) h) (/ scale blh))
       p0 (polar p0 pi18 (/ dh 2.0))
  ) ;setq end
)   ;if end
 (setq p0 (polar p0 pi09 (+ ll (* 15 scale) (* -1 b)))
 );setq end
(setq cg jcbg)
(if (> n-len 1) 
 (progn 
  (setq m 1)
  (repeat (- n-len 1)
   (setq l (nth m l-l) h (nth m l-h) )
   (setq cg (+ cg (/ (nth (1- m) l-l) 1000.0))
         txt-cg (if (= j-cc "1") (rtos cg 2 2) txt-cg)
         j-dc (if (= m (1- n-len)) 1 0)
   )
   (draw_zhu l h p0 txt-cg j-dc)
   (setq dh (* (- (nth m l-h) h) (/ scale blh)))
   (setq p0 (polar p0 pi27 (/ dh 2.0))
         p0 (polar p0 pi09 l)
         m (1+ m)
   ) ;setq end
  ) ;repeat end
 ) ;progn end
)  ;if end
   (setq cg (+ cg (/ (nth (1- n-len) l-l) 1000.0)) 
         txt-cg (if (= j-cc "1") (rtos cg 2 2) txt-cg)
         j-dc (if (= m (1- n-len)) 1 0)
   ) ;setq end
   (draw_zt p0 txt-cg)
(setvar "cmdecho" 1)
(command "layer" "s" "0" "")
) ;defun end
;

       


