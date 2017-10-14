;--------------------------------------+
;文件名：M-AS.lsp                      +
；计算梁板受弯配筋计算　　　　　　　 　+
;--------------------------------------+
(defun c:M-AS( / c lx b h gj fy ksib m mode fc h0 tt w as) 
 (inivar) ;初始化系统变量
 (textscr)
 (setq c (if (setq temp (getreal "\n砼强度等级:<20>"))  temp 20) 
       lx (if (= (setq temp (getstring "计算板(1)/梁(2):<1>")) "") "1" temp)
       b (if (= lx "2") (getreal "梁宽(mm)：") 1000)
       h (if (= lx "1") (getreal "板厚(mm):") 
                        (getreal "梁高(mm):"))
       gj (if (setq temp (getreal "钢筋等级:<2>"))  temp 2)
       fy (if (= gj 1) 210 310)
       ksib (if (= gj 1) 0.614 0.544) ;极限受压区高度
       m (getreal "弯矩(KN-m):")
       mode (getint "\n按旧规范GB10-89(0)/按新规范GB50010-2002(1)计算:<1>")
       mode (if mode mode 1)
 ) ;setq end
 (if (null (type concrete)) (load "concrete.lsp")) 
 (setq fc (concrete "fc" c mode)) ;查询混凝土轴心受压强度
 (if (= mode 1) (setq fc (* fc (alfa1 c)))) ;如果按新规范
 (setq h0 (if (= lx "1") (- h 20) (- h 35)) ;计算有效受压区高度
       tt (/ (* m 1e6) (* fc b h0 h0))      
       w (- 1 (sqrt (- 1 (* 2.0 tt))))
       as (/ (* fc b h0 w) fy ) 
 ) ;setq end
 (if (<= w ksib) 
   (progn 
      (princ (strcat "\n相对受压区高度= " (rtos w 2 4)))
      (princ (strcat "\n配筋面积= " (rtos as 2 2) "(mm2)" ))
   ) ;progn end
   (progn 
      (princ (strcat "\n相对受压区高度=" (rtos w 2 4) " >极限受压区高度=" (rtos ksib 2 4)))
      (princ "\n截面不够,请加大截面!")
   ) ;progn
 ) ;if end
 (resvar) ;还原系统变量 
 (princ)
) ;defun end
