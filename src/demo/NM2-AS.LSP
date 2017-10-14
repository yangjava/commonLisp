;+----------------------------------+
;+双向偏心受压构件偏心矩增大系数计算+ 
;+----------------------------------+

;根据给定条件按单向偏压求解对称配筋
(defun as1(b h l0 M N yei mode fcm fy / pxin e as1 as2 ksi C h0 x C a as txt)
 (setq  h0 (- h 40)
   	ksib 0.544	
	ksi (/ N fcm b h0)
	pxin "\n大偏心 x/h0="
        e (+ yei (* 0.5 h) -40)
	x (* ksi h0)
	x (if (< x 80) 80 x)
	as1 (+ yei (* -0.5 h) (* 0.5 x))
	as1 (* N as1)
	as2 (* fy (- h0 40))
	as  (/ as1 as2)
 ) ;setq end
 (if (= mode 0)
     (setq l_fac1 0.45  l_fac2 0.8) ;旧规范
     (setq l_fac1 0.43  l_fac2 (beta1 c)) ;新规范
 ) ;if end    
 (if (> ksi ksib) ;小偏心
     (setq as1 (- (* N e) (* l_fac1 fcm b h0 h0))
           as2 (* (- l_fac2 ksib) (- h0 40))
	   as1 (/ as1 as2)
	   as2 (* fcm b h0)
	   as1 (+ as1 as2)
	   as2 (- N (* ksib fcm b h0))
	   ksi (/ as2 as1)
	   ksi (+ ksi ksib)
	   pxin "\n小偏心 x/h0="
       	   C (- ksi (* 0.5 ksi ksi))
           a (* fy (- h0 40))
	   Asc (* fcm b h0 h0)
           as1 (- (* N e) (* Asc C))
           as (/ as1 a)
     ) ;setq end
  ) ;if end
  (setq txt (strcat pxin (rtos ksi 2 4) "  Ase= " (rtos as 2 1) "(mm2)" ))
  (princ txt)
  (setq as as)
) ;defun end	

(defun yta(b h l0 M N fc / yta1 yta2 dyta yta txt-yta1 txt-yta2 txt-dyta txt-yta h0)
 (setq  e0 (/ M N )
        h0 (- h 40)
      	ea (if (<= e0 (* 0.3 h0)) (* 0.12 (- (* 0.3 h0) e0)) 0)
	ei (+ e0 ea)
	eih0 (/ ei h0)
	yta1 (* 0.5 fc B H (/ 1.0 N))
  	yta1 (if (> yta1 1.0) 1 yta1)
	yta2 (/ l0 h 100)
	yta2 (- 1.15 yta2)
	yta2 (if (< (/ l0 h) 15) 1.0 yta2)
	dyta (* (/ l0 h) (/ l0 h))
	dyta (/ dyta 1400 eih0)
	yta (+ 1 (* dyta yta1 yta2))
	yta (if (<= (/ l0 h) 8) 1.0 yta)
	txt-yta1 (strcat "\nyta1=" (rtos yta1 2 4))
	txt-yta2 (strcat "\nyta2=" (rtos yta2 2 4))
	txt-dyta (strcat "\ndyta=" (rtos dyta 2 4))
	txt-yta  (strcat "\nyta=1+dyta*yta1*yta2=" (rtos yta 2 4))
 ) ;setq end
 (princ txt-yta1)
 (princ txt-yta2)
 (princ txt-dyta)
 (princ txt-yta)
 (setq yta (* ei yta))
) ;defun end

(defun c:nm2-as()
 (textscr)
 (inivar) ;初始化系统变量
 (load "tab.lsp")
 (load "concrete.lsp")
 (setq scale 1)
 (princ "\n          ^	X	")
 (princ "\n          |  	")
 (princ "\n     +----|----+	")
 (princ "\n     |    |    |	")
 (princ "\n     |    |    | Asy ")
 (princ "\n   --+----+----+----> Y ")
 (princ "\n     |    |    |	")
 (princ "\n     |    |    |	")
 (princ "\n     +----|----+	")
 (princ "\n	     Asx	")
 (princ "\n")
 (princ "\n本程序用于求解对称配筋矩形截面双向偏心受压构件配筋")
 (princ "\n钢筋内定为II级钢筋,如按I级钢筋,应乘以系数310/210")
 (princ "\nb,h--截面高，宽; l0--构件计算长度; ")
 (princ "\nN--设计轴力，e0x e0y--x,y向初始偏心;")
 (princ "\nc--砼等级， fy--钢筋受拉强度")
 (PRINC "\nb(mm),h(mm),l0(m),N(KN),e0x(mm),e0y(mm),c,fy\n:")
 (setq l-l (readlh) 
       mode (getint "\n按旧规范GB10-89(0)/按新规范GB50010-2002(1)计算:<1>")
       mode (if mode mode 1)
       b (nth 0 l-l)
       h (nth 1 l-l)
       b0 (- b 40)
       h0 (- h 40)  
       l0 (* (nth 2 l-l) 1000)
       N (* (nth 3 l-l) 1000)
       e0x (nth 4 l-l) 
       e0y (nth 5 l-l) 
       M0x (* N e0x)
       M0y (* N e0y)
       c (nth 6 l-l)
       fcm (if (= mode 0) (concrete "fcm" c mode)  ;旧规范
                          (* (alfa1 c) (concrete "fc" c mode))  ;新规范
           ) ;if end
       fc (concrete "fc" c mode)	
       fy (nth 7 l-l)
       ksib 0.544
       yeix (yta b h l0 M0x N fc)
       yeiy (yta h b l0 M0y N fc)
       yeixh (/ yeix h0)
       yeiyb (/ yeiy b0)
       addy (+ yeixh yeiyb)
       psix (/ yeixh addy)
       psiy (/ yeiyb addy)
       Nex (/ N psix)
       Ney (/ N psiy)
       txt1 (strcat "\npsix=" (rtos psix 2 3) " Nex=" (rtos (/ Nex 1000) 2 2) " KN")
       txt2 (strcat "\npsiy=" (rtos psiy 2 3) " Ney=" (rtos (/ Ney 1000) 2 2) " KN")
 ) ;setq end
 (princ txt1)
 (princ txt2)
 (princ "\nx方向计算：")
 (Setq Asex0 (* 0.2 b h0 fcm (/ 1.0 fy))
	txt1 (strcat "\nAsex0=0.2*b*h0*fcm/fy= " (rtos Asex0 2 2) " mm2")
	temp t)
 (princ txt1)
 (while temp ;x方向迭代求Asex
     (setq 
	Nu0x (+ (* fc b h) (* 2 fy Asex0))
	d_Nu0x (/ 0.5 Nu0x)
	d_Nex (/ 1.0 Nex)
	Nux (/ 1.0 (+ d_Nex d_Nu0x))
	Asex (as1 b h l0 M0x Nux yeix mode fcm fy)
	delta (abs (- Asex Asex0))
	temp (if (< delta (* Asex 0.001)) nil t)
	Asex0 Asex
      ) ;setq end
 ) ;while end
 (setq txt1 (strcat "\nNux= " (rtos (/ Nux 1000) 2 2) " KN Asex=" (rtos Asex 2 2) " mm2"))
 (princ txt1)
 (princ "\ny方向计算：")
 (Setq Asey0 (* 0.2 h b0 fcm (/ 1.0 fy))
	txt1 (strcat "\nAsey0=0.2*h*b0*fcm/fy= " (rtos Asey0 2 2) " mm2")
	temp t)
 (princ txt1)
 (while temp  ;y方向迭代求Asex
     (setq 
	Nu0y (+ (* fc b h) (* 2 fy Asey0))
	d_Nu0y (/ 0.5 Nu0y)
	d_Ney (/ 1.0 Ney)
	Nuy (/ 1.0 (+ d_Ney d_Nu0y))
	Asey (as1 h b l0 M0y Nuy yeiy mode fcm fy)
	delta (abs (- Asey Asey0))
	temp (if (< delta (* 0.001 Asey)) nil t)
	Asey0 Asey
      ) ;setq end
 ) ;while end
 (setq txt1 (strcat "\nNuy= " (rtos (/ Nuy 1000) 2 2) " KN Asey=" (rtos Asey 2 2) " mm2"))
 (princ txt1)
 (if (> yeixh 0.5)
	(Setq ksix (- 0.36 (/ 0.13 yeixh)))
	(Setq ksix (- 0.50 (* 0.80 yeixh)))
 ) ;if end
 (if (> yeiyb 0.5)
	(Setq ksiy (- 0.36 (/ 0.13 yeiyb)))
	(Setq ksiy (- 0.50 (* 0.80 yeiyb)))
 ) ;if end
 (Setq kk1 (- 1.0 (* 4 ksix ksiy))
       d_rei (getreal "\n输入角筋直径<20>:")
       d_rei (if d_rei d_rei 20)
       Asc (* 0.25 pi d_rei d_rei)			
       kk2 (- Asex (* 2 ksix Asey) 
	           (* 2 Asc (- 1.0 (* 2 ksix))))
       kk3 (- Asey (* 2 ksiy Asex)
	           (* 2 Asc (- 1.0 (* 2 ksiy))))
       Aswx (/ kk2 kk1)
       Aswy (/ kk3 kk1)
       txt1 (strcat "\nksix=" (rtos ksix 2 3) " 垂直x方向两对边腹部筋 Aswx=" (rtos Aswx 2 2) " mm2")	
       txt2 (strcat "\nksiy=" (rtos ksiy 2 3) " 垂直y方向两对边腹部筋 Aswy=" (rtos Aswy 2 2) " mm2")	
       txt3 (strcat "\n角部筋 Asc=" (rtos Asc 2 2) " mm2")
 ) ;setq end
 (princ txt3)
 (princ txt1)
 (princ txt2)
 (resvar) ;还原系统变量
 (princ)
) ;defun end	
 





