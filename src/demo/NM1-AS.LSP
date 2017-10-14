;+----------------------------------+
;+单向偏心受压构件偏心矩增大系数计算+ 
;+----------------------------------+

(defun yta(b h l0 M N c mode / yta1 yta2 dyta yta txt-yta1 txt-yta2 txt-dyta txt-yta)
 (setq  e0 (/ M N )
      	ea (if (<= e0 (* 0.3 h0)) (* 0.12 (- (* 0.3 h0) e0)) 0)
	ei (+ e0 ea)
	h0 (- h 40)
	eih0 (/ ei h0)
	fc (concrete "fc" c mode)
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
) defun end

(defun c:NM1-AS( / b h l0 N M c fy ksib yei ksi pxin e x as1 as2 as a Asc txt)
 (inivar) ;初始化系统变量
 (textscr)
 (load "tab.lsp")
 (load "concrete.lsp")
 (setq scale 1)
 (princ "\n本程序用于求解对称配筋矩形截面偏心受压构件配筋")
 (princ "\n钢筋内定为II级钢筋,如按I级钢筋,应乘以系数310/210")
 (princ "\nb,h--截面高，宽; l0--构件计算长度; ")
 (princ "\nN,M--设计轴力，弯矩; c--砼等级")
 (PRINC "\nb(mm),h(mm),l0(m),N(KN),M(KN-m),c\n:")
 (setq l-l (readlh) 
       mode (getint "\n按旧规范GB10-89(0)/按新规范GB50010-2002(1)计算:<1>")
       mode (if mode mode 1)
       c (nth 5 l-l)
       fcm (if (= mode 0) (concrete "fcm" c mode)  ;旧规范
                          (* (alfa1 c) (concrete "fc" c mode))  ;新规范
           ) ;if end
       b (nth 0 l-l)
       h (nth 1 l-l)
       h0 (- h 40) 	
       l0 (* (nth 2 l-l) 1000)
       N (* (nth 3 l-l) 1000)
       M (* (nth 4 l-l) 1e6)
       fy 310
       ksib 0.544	
       yei (yta b h l0 M N c mode)
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
  (setq txt (strcat pxin (rtos ksi 2 4) "  Ase= " (rtos as 2 2) "(mm2)" ))
  (princ txt)
  (resvar) ;还原系统变量
  (princ)
) ;defun end	