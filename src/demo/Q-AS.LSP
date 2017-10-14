;+-----------------------+
;+ 计算梁柱受剪承载力    +
;+ 文件名Q-AS.lsp        +
;+-----------------------+
;计算混凝土强度影响系数
(defun betac(n / Bc)
 (cond
   ((<= n 50) (setq Bc 1.0))
   ((= n 80)  (setq Bc 0.8))
   (T (setq Bc (- 1.0 (* (/ 0.2 30) (- n 50))))) ;插值
 ) ;cond end
) ;defun end

;验算截面尺寸条件V1=0.25*Bc*fc*b*h0 
;砼受剪承载力Vc=0.7*ft*b*h0 (当lamda=0)
;砼受剪承载力Vc=1.75*fc*b*h0/(lamda+1) (当lamda>0)
;箍筋受剪承载力Vs=1.25*fyv*Asv*h0/s (当lamda=0)
;箍筋受剪承载力Vs=fyv*Asv*h0/s (当lamda>0)
;弯起筋受剪承载力Vw=0.8*fy*Asb*sin(alfa)
;轴向压力受剪承载力VN=0.07N
;轴向拉力受剪承载力VN=-0.2N
;V<=Vc+Vs+Vw+VN
(defun c:q-as()
 (inivar) ;初始化系统变量
 (textscr)
 (load "concrete.lsp")
 (load "tab.lsp")
 (setq scale 1 temp T)
 (while temp
  (princ "\n")
  (princ "\n请输入以下数据，如lamda、N没有，可输入0")
  (princ "\nb,h--截面宽，高；V,N--设计剪力值,轴力(受压为正)；")
  (princ "\nc--砼标号；lamda-- 剪跨比")
  (princ "\n请输入b(mm),h(mm),V(KN),c,lamda,N (KN)")
  (princ "\n:")
  (setq l-l (readlh)
        len (length l-l)
  ) ;setq end
  (if (< len 5) 
     (princ "\n参数输入错，请重新输入：")
     (progn 
      (setq temp nil
            b (nth 0 l-l)
	    h (nth 1 l-l)
	    h0 (- h 35)	
	    V (nth 2 l-l)
	    c (nth 3 l-l)
  	    ft (concrete "ft" c 1) ;按新规范计算
  	    fc (concrete "fc" c 1) ;按新规范计算
	    lamda (nth 4 l-l)
            lamda (cond
                     ((and (< lamda 1.5) (> lamda 0.01)) 1.5)
                     ((> lamda 3.0) 3.0)
                     (T lamda)
                  ) ;cond end
	    N (nth 5 l-l)
            N (if (> N (* 0.3 fc b h)) (* 0.3 fc b h) N) ;N不能大于0.3*fc*bh
            Bc (betac c)
            V1 (* 0.25 fc b h0 0.001 Bc)
	    Vc (if (> lamda 0) 
                   (/ (* 1.75 ft b h0) (+ lamda 1.0))
                   (* 0.7 ft b h0)
                ) ;if end
	    Vc (* Vc 0.001)
            txt1 (if (>= V V1)
	          (strcat "\nV1=0.25*Bc*fc*b*h0=" (rtos V1 2 2) " < V=" (rtos V 2 2) "   加大截面! 重新输入")
	          (strcat "\nV1=0.25*Bc*fc*b*h0=" (rtos V1 2 2) " > V=" (rtos V 2 2) "   截面合适!")
     	         ) ;if end
    	    txt0 (if (> lamda 0)
                  (strcat "\n砼受剪承载力Vc=1.75*ft*b*h0/(lamda+1.0)=" (rtos Vc 2 2) "(mm)") 
                  (strcat "\n砼受剪承载力Vc=0.7*ft*b*h0=" (rtos Vc 2 2) "(mm)") 
                 ) ;if end
        ) ;setq end
      (princ txt1)
      (princ "\n")
      (if (>= V V1) 
          (setq temp T)
          (progn 
	    (setq temp1 T)
            (while temp1
 	     (princ "\n如不设弯起钢筋，Asb,a可输入0")
             (princ "\nd-箍筋直径(mm) S-箍筋间距(mm) n-箍筋肢数 fy-箍筋等级")
             (princ "\nAsb-弯起钢筋面积(mm2) a-弯起钢筋角度(度)")
             (princ "\n输入d,S,n,fy,Asb,a:")
             (setq l-l (readlh))
             (if (< (length l-l) 6)
                 (princ "\n参数输入错，请重新输入：")
                 (progn
	         (setq temp1 nil
                       d (nth 0 l-l)
		       As (* pi d d 0.25)	
                       s (nth 1 l-l)
                       ng (nth 2 l-l)
		       Asv (* As ng)
                       fy (nth 3 l-l)
		       fy (if (= fy 1) 210 310)
                       Asb (nth 4 l-l)
                       a (nth 5 l-l)
		       a (* a pi (/ 1.0 180))
		       Vs (if (> lamda 0) 
                              (* fy Asv h0 (/ 1 s) 0.001)
                              (* 1.25 fy Asv h0 (/ 1 s) 0.001)
                          ) ;if end
		       txt (if (> lamda 0) 
                             (strcat "\n箍筋受剪承载力Vs=fyv*Asv*h0/s=" (rtos Vs 2 2) "(KN)")
                             (strcat "\n箍筋受剪承载力Vs=1.25*fyv*Asv*h0/s)=" (rtos Vs 2 2) "(KN)")
                           ) ;if end
 	               Vw (* 0.8 310 Asb (sin a) 0.001)
		       txt1 (strcat "\n弯起钢筋受剪承载力Vw=0.8*310*Asb*sin(a)=" (rtos Vw 2 2) "(KN)")
		       VN (if (> N 0) (* 0.07 N) (* 0.2 N))
		       txt2 (if (> N 0)
                             (strcat "\n轴向压力受剪承载力VN=0.07*N=" (rtos VN 2 2) "(KN)")
                             (strcat "\n轴向拉力受剪承载力VN=0.2*N=" (rtos VN 2 2) "(KN)")
                            ) ;if end
		       VV (+ Vc Vs Vw VN)
		       txt3 (if (>= VV V)
			    (strcat "\nV=" (rtos V 2 2) "(mm) < VC+Vs+Vw+VN= " (rtos VV 2 2) "  合格!")
			    (strcat "\nV=" (rtos V 2 2) "(mm) > VC+Vs+Vw+VN= " (rtos VV 2 2) " 不合格!\n请重新输入:")
			   ) ;if end
                       temp1 (if (>= VV V) nil T)
                  ) ;setq end	
		  (princ txt0)
                  (princ txt)
		  (princ txt1)
		  (princ txt2)
		  (princ txt3)
             ) ;progn end
           ) ;if (< (length l-l) 6) end
          ) ;while temp1 end
        ) ;progn end
       ) ;if (> V V1) end
      ) ;progn end
     ) ;if (< len 5) end
    ) ;while temp end
  (resvar) ;还原系统变量
  (princ)
);defun end
