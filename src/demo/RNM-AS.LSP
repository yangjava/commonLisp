;+--------------------------------------+
;+圆形截面偏心受压构件偏心矩增大系数计算+ 
;+--------------------------------------+
(defun fun(N a / at pia piat X1 X2 X3 X4 Y1 Y2 Area f)
  (setq at (att a)
	pia (* pi a)
	piat (* pi at)
	X1 (sin (* 2 pia))
	X1 (/ X1 2 pia)
	X0 (- 1 X1)
	Area (* pi R R)
	X1 (* a fcm Area X0)
	txt (strcat "a*fcm*Area*X0=" (rtos a 2 3) "X" (rtos fcm 2 1) "X" (rtos Area 2 2) "X" 
                    (rtos X0 2 3) " =" (rtos X1 2 3))
  ) ;setq end
;  (princ txt)
  (setq
	Y1 (- N X1)
 	Y1 (/ Y1 (- a at))
	X2 (sin pia)
	X22 X2
	X4 (* X2 X2 X2)
	X2 (* (/ 2.0 3.0 pi) fcm Area R X4)
   ) ;setq end
 ;  (princ "\n2/3fcm*Area*...=")
 ;  (princ X2)
  (setq
	X2 (- (* N yei) X2)
	X3 (sin piat)
	X3 (+ X22 X3)
	Y3 (* Rs X3 (/ 1.0 pi))
	Y2 (/ X2 Y3)
 ) ;setq end
 (setq 	f (- Y1 Y2))
) ;defun end


(defun att(a)
 (setq at (- 1.25 (* 2 a))
       at (if (> a 0.625) 0 at)
 ) ;setq end
 (setq at at)
) ;defun end

(defun yta(b h l0 M N c mode / yta1 yta2 dyta yta txt-yta1 txt-yta2 txt-dyta txt-yta Area)
 (if (null (type concrete)) (load "concrete.lsp"))
 (setq 	e0 (/ M N )
      	ea (if (<= e0 (* 0.3 h0)) (* 0.12 (- (* 0.3 h0) e0)) 0)
	ei (+ e0 ea)
	eih0 (/ ei h0)
	fc (concrete "fc" c mode)
	Area (* 0.25 pi h h)
	yta1 (* 0.5 fc Area (/ 1.0 N))
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

;根据求得的a计算配筋As
(defun fun-as(N a / at pia piat X1 Y1 Area as)
  (setq at (att a)
	pia (* pi a)
	piat (* pi at)
	X1 (sin (* 2 pia))
	X1 (/ X1 2 pia)
	X0 (- 1 X1)
	Area (* pi R R)
	X1 (* a fcm Area X0)
	txt (strcat "a*fcm*Area*X0=" (rtos a 2 3) "X" (rtos fcm 2 1) "X" (rtos Area 2 2) "X" 
                    (rtos X0 2 3) " =" (rtos X1 2 3))
  ) ;setq end
;  (princ txt)
  (setq
	Y1 (- N X1)
 	Y1 (/ Y1 (- a at))
  ) ;setq end
 (setq 	as (/ Y1 fy))
) ;defun end
 

(defun c:Rnm-as()
 (textscr)
 (inivar) ;初始化系统变量
 (load "tab.lsp")
 (load "concrete.lsp")
 (setq scale 1)
 (princ "\n本程序用于求解沿周边均匀配筋圆形截面偏心受压构件配筋")
 (princ "\n钢筋内定为II级钢筋,如按I级钢筋,应乘以系数310/210")
 (princ "\nc--砼等级")
 (PRINC "\nR(mm),l0(m),N(KN),M(KN-m),c\n:")
 (setq l-l (readlh) 
       mode (getint "\n按旧规范GB10-89(0)/按新规范GB50010-2002(1)计算:<1>")
       mode (if mode mode 1)
       R (nth 0 l-l)
       Rs (- R 35)
       h (* 2 R)
       h0 (+ R Rs) 	
      l0 (nth 1 l-l)
      l0 (* l0 1000)	
      N (nth 2 l-l)
      N (if (< N 0.1) 0.1 N)
      N (* N 1000)
      M (* (nth 3 l-l) 1e6)
      c (nth 4 l-l)
      fcm (if (= mode 0) (concrete "fcm" c mode)  ;旧规范
                          (* (alfa1 c) (concrete "fc" c mode))  ;新规范
          ) ;if end
      fy 310
      ksib 0.544	
      yei (yta b h l0 M N c mode)
 ) ;setq end
;----画出函数图形
(graphscr)
(command "erase" "w" "0,-10" "3,10" "")
(setq p0 '(0 0) p1 '(3 0) p2 '(0 2) dl 3 x0 0.01)
(command "line" p0 p1 "")
(command "line" p0 p2 "")
(setq y0 (fun N x0) 
      y0 (/ y0 1e8) ;将y0缩小1e8倍
	x0 (* dl x0)
      p1 (list x0 y0)
      x1 (+ x0 0.01)
) ;setq end
(repeat 100
 (setq y1 (fun N x1)
	y1 (if (> (abs y1) 1e9) 1e9 y1)
	y1 (/ y1 1e8) ;将y1缩小1e8倍
	x0 (* dl x1)
	p2 (list x0 y1)
	x1 (+ x1 0.01)
 );setq end
 (command "line" p1 p2 "")
 (setq p1 p2)
) ;repeat end

;----放大图形
 (command "zoom" "w" "0,-10" "3,10")
 (princ "\n输入放大两点:")
 (command "zoom" "w" pause pause)
 (princ "\n点取计算区间")
  (setq  p1 (getpoint "\n第一点:")
	 a1 (/ (nth 0 p1) 3.0)
	p2 (getpoint "\n第二点:")
	a2 (/ (nth 0 p2) 3.0)
	da (abs (- a2 a1)) 
   )
 (princ "\n二分法迭代:")
 (while   (> da 0.0001)  
  (setq f1 (fun N a1)
	f2 (fun N a2)
	a3 (* 0.5 (+ a1 a2))
	f3 (fun N a3)
	temp (if (> (* f1 f2) 0) T nil)
	txt1 (strcat "\nx1=" (rtos a1 2 3) "  f=" (rtos f1 2 3))
	txt2 (strcat "\nx2=" (rtos a2 2 3) "  f=" (rtos f2 2 3))
	txt3 (strcat "\nx3=" (rtos a3 2 3) "  f=" (rtos f3 2 3))
  );setq end
  (princ (strcat "\na=" (rtos a3 2 4) " f(a)=" (rtos f3 2 4)))
  (if (< (* f1 f3) 0)
     (setq a1 a1
           a2 a3
           da (abs (- a2 a1))
     ) ;setq end
  ) ;if end
  (if (< (* f2 f3) 0)
      (setq a1 a3
            a2 a2
            da (abs (- a2 a1))
     ) ;setq end
  ) ;if end
 ) ;while end
 (princ "\n结果：")
 (setq as (fun-as N a3)
       txt1 (strcat "\na=" (rtos a3 2 4) "   As=" (rtos as 2 2) "mm2")
 ) ;setq end
 (princ txt1)
 (resvar) ;还原系统变量
 (princ)
) ;defun end	