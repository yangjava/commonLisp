;+------------------------+
;+ 最大裂缝宽度计算   	  + 
;+------------------------+
(vmon)
(defun c:wmax()
 (inivar) ;初始化系统变量
 (textscr)
 (load "tab.lsp")
 (load "concrete.lsp")
 (setq scale 1)
 (princ)
 (princ "输入:截面宽B(mm),截面高H(mm),钢筋直径d(mm),截面钢筋AS(mm2),砼强度等级c\n:")
 (setq l-l (readlh)
       b (nth 0 l-l)
       h (nth 1 l-l)
       h0 (- h 35)
       d (nth 2 l-l)
       As (nth 3 l-l)
       c  (nth 4 l-l)
       ft (concrete "ft" c 1) ;按新规范计算
       ftk (/ ft 0.7333333)
  ) ;setq end
  (princ "输入截面短期荷载效应下内力(用荷载标准值计算),如某项没有可输0")
  (princ "\n其中Ns受拉为正，受压为负")
  (princ "\nMs(KN-m),Ns(KN-m):")
  (setq l-l (readlh)
        Ms (nth 0 l-l)
  	Ns (nth 1 l-l)
  ) ;setq end
  (if (and (<= Ms 0) (> Ns 0))
      (setq pte (/ As B H))  ;轴心受拉
      (setq pte (/ As 0.5 B H)) ;受弯,偏心受拉
  ) ;if end
  (if (< pte 0.01) (setq pte 0.01))
  (cond 
     ((<= Ms 0) (setq sigmas (/ (* Ns 1000) As) acr 2.7)) ;轴心受拉
     ((<= Ns 0) (setq sigmas (/ (* Ms 1e6) 0.87 h0 As) acr 2.1)) ;受弯
     (T (setq e0 (/ Ms Ns 0.001)  ;偏心受拉
	      e1 (+ e0 (* 0.5 h) -35)
              sigmas (/ (* Ns e1 1e3) As (- h0 35)) 
	      acr 2.4
        ) ;setq end
     )
  ) ;cond end
  (setq temp (getreal "\n变形钢筋(1)/光面钢筋(2):<1>")
        temp (if temp temp 1)
        u (if (= temp 1) 0.7 1.0)
        Es (if (= temp 1) 2.0e5 2.1e5)
        psia (* 0.65 ftk)
        psia (/ psia pte sigmas)
	psia (- 1.1 psia)
        psia (if (< psia 0.4) 0.4 psia)
   	psia (if (> psia 1.0) 1.0 psia)
	c (* 2.7 25)
	w1 (* acr psia sigmas (/ 1.0 Es))
        w2 (/ d pte 10)
 	w2 (+ c w2)
	Wmax (* w1 w2 u)
  ) ;setq end
  (setq txt-pte (strcat "\n纵向受拉钢筋配筋率pte= " (rtos pte 2 4))
        txt-s   (strcat "\n荷载短期效应组合下受拉钢筋应力sigmas= " (rtos sigmas 2 2) "N/mm2")
        txt-p   (strcat "\n应变不均匀系数psia= " (rtos psia 2 4))
        txt-l   (strcat "\n构件受力特征系数acr= " (rtos acr 2 1))
        txt-u   (strcat "\n纵向受拉钢筋表面特征系数u= " (rtos u 2 1))
        txt-W   (strcat "\n最大裂缝宽度Wmax= " (rtos Wmax 2 4) "(mm)")
  ) ;setq end
  (princ txt-pte)
  (princ txt-p)
  (princ txt-l)
  (princ txt-u)
  (princ txt-s)
  (princ txt-w)
  (resvar) ;还原系统变量
)  ;defun end
  