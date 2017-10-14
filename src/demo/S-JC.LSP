;判别参数var3在[var1 var2]的哪个范围:
;   var3<var1            返回1
;   var3>var2            返回3
;var3在[var1 var2]区间内 返回2
(defun comp(var1 var2 var3 / temp)
   (if (< var3 var1) (setq temp 1))
   (if (> var3 var2) (setq temp 3))
   (if (and (<= var3 var2) (>= var3 var1)) (setq temp 2))
   (setq temp temp)
) ;defun end

;根据计算深度(zb)查表、插值计算平均附加应力系数
(defun func(zb jn / temp n1 n2  l-jn ind temp c11 c12 c21 c22 jn1 jn2 dc1 dc2
		 djn zb1 zb2 c1 c2 zb1 zb2 l-jn jn1 jn2 c dc)
   (setq n1 (+ (fix (* zb 10)) 1000) ;计算表格组码
         n2 (1+ n1)
         tab1 (cdr (assoc n1 l_tab))
         tab2 (cdr (assoc n2 l_tab))
         l-jn '(1.0 1.2 1.4 1.6 1.8 2.0 2.4 2.8 3.2 3.6 4.0 5.0 10.0)
         temp t
         ind 1
   ) ;setq end
   (while temp   ;根据基础长宽比计算查表列数
      (setq at (nth ind l-jn))
      (if (>= at jn) 
 	(setq temp nil) 
	(setq ind (1+ ind))
      ) ;if end
   ) ;while end
   (setq c11 (nth (1- ind) tab1)  ;插值
  	 c12 (nth ind tab1)
         c21 (nth (1- ind) tab2)
	 c22 (nth ind tab2)
         zb1 (/ (- n1 1000) 10.0)
         zb2 (+ zb1 0.1)
         jn1 (nth (1- ind) l-jn)
         jn2 (nth ind l-jn)
         dc1 (- c12 c11)
         dc2 (- c22 c21)
         djn (- jn2 jn1)
         dc1 (/ dc1 djn)
         dc2 (/ dc2 djn)
         dc1 (* dc1 (- jn jn1))
         dc2 (* dc2 (- jn jn1))
         c1 (+ c11 dc1)
         c2 (+ c21 dc2)
         dc (- c2 c1)
         c (+ c1 (* dc (- zb zb1) 10))
  ) ;setq end
  (setq c c)
);defun end

(defun c:s-jc()
 ;用分层总和法计算基础沉降
 (inivar) ;初始化系统变量
 (setq l_tab (list
  '(1000 1.000 1.000 1.000 1.000 1.000 1.000 1.000 1.000 1.000 1.000 1.000 1.000 1.000) 
  '(1001 0.997 0.998 0.998 0.998 0.998 0.998 0.998 0.998 0.998 0.998 0.998 0.998 0.998) 
  '(1002 0.987 0.990 0.992 0.992 0.992 0.992 0.993 0.993 0.993 0.993 0.993 0.993 0.993) 
  '(1003 0.967 0.973 0.976 0.978 0.979 0.979 0.980 0.980 0.981 0.981 0.981 0.981 0.981)
  '(1004 0.936 0.947 0.953 0.956 0.958 0.965 0.961 0.962 0.962 0.963 0.963 0.963 0.963)

  '(1005 0.900 0.915 0.924 0.929 0.933 0.935 0.937 0.939 0.939 0.940 0.940 0.940 0.940)
  '(1006 0.858 0.878 0.890 0.898 0.903 0.906 0.910 0.912 0.913 0.914 0.914 0.915 0.915)
  '(1007 0.816 0.840 0.855 0.865 0.871 0.876 0.881 0.884 0.885 0.886 0.887 0.887 0.888)
  '(1008 0.775 0.801 0.819 0.831 0.839 0.844 0.851 0.855 0.857 0.858 0.859 0.860 0.860)
  '(1009 0.735 0.764 0.784 0.797 0.806 0.813 0.821 0.826 0.829 0.830 0.831 0.832 0.833)
  
  '(1010 0.698 0.728 0.749 0.764 0.775 0.783 0.792 0.798 0.801 0.803 0.804 0.806 0.807)
  '(1011 0.663 0.694 0.717 0.733 0.744 0.753 0.764 0.771 0.775 0.777 0.779 0.780 0.782)
  '(1012 0.631 0.663 0.686 0.703 0.715 0.725 0.737 0.744 0.749 0.752 0.754 0.756 0.758)
  '(1013 0.601 0.633 0.657 0.674 0.688 0.698 0.711 0.719 0.725 0.728 0.730 0.733 0.735)
  '(1014 0.573 0.605 0.629 0.648 0.661 0.672 0.687 0.696 0.701 0.705 0.708 0.711 0.714)

  '(1015 0.548 0.580 0.604 0.622 0.637 0.648 0.664 0.673 0.679 0.683 0.686 0.690 0.693)
  '(1016 0.524 0.556 0.580 0.599 0.613 0.625 0.641 0.651 0.658 0.663 0.666 0.670 0.675)
  '(1017 0.502 0.533 0.558 0.577 0.591 0.603 0.620 0.631 0.638 0.643 0.646 0.651 0.656)
  '(1018 0.482 0.513 0.537 0.556 0.571 0.583 0.600 0.611 0.619 0.624 0.629 0.633 0.638)
  '(1019 0.463 0.493 0.517 0.536 0.551 0.563 0.581 0.593 0.601 0.606 0.610 0.616 0.622)

  '(1020 0.446 0.475 0.499 0.518 0.533 0.545 0.563 0.575 0.584 0.590 0.594 0.600 0.606)
  '(1021 0.429 0.459 0.482 0.500 0.515 0.528 0.546 0.559 0.567 0.574 0.578 0.585 0.591)
  '(1022 0.414 0.443 0.466 0.484 0.499 0.511 0.530 0.543 0.552 0.558 0.563 0.570 0.577)
  '(1023 0.400 0.428 0.451 0.469 0.484 0.496 0.515 0.528 0.537 0.544 0.548 0.556 0.564)
  '(1024 0.387 0.414 0.436 0.454 0.469 0.481 0.500 0.513 0.523 0.530 0.535 0.543 0.551)

  '(1025 0.374 0.401 0.423 0.441 0.455 0.468 0.486 0.500 0.509 0.516 0.522 0.530 0.539)
  '(1026 0.362 0.389 0.410 0.423 0.442 0.455 0.473 0.487 0.496 0.505 0.509 0.518 0.528)
  '(1027 0.351 0.377 0.398 0.416 0.430 0.442 0.461 0.474 0.484 0.492 0.497 0.506 0.517)
  '(1028 0.341 0.366 0.387 0.404 0.418 0.430 0.449 0.463 0.472 0.480 0.486 0.495 0.506)
  '(1029 0.331 0.356 0.377 0.393 0.407 0.419 0.438 0.451 0.461 0.469 0.475 0.485 0.496)

  '(1030 0.322 0.346 0.366 0.383 0.397 0.409 0.427 0.441 0.451 0.459 0.465 0.474 0.487)
  '(1031 0.313 0.337 0.357 0.373 0.387 0.398 0.417 0.430 0.440 0.448 0.454 0.464 0.477)
  '(1032 0.305 0.328 0.348 0.364 0.377 0.389 0.407 0.420 0.431 0.439 0.445 0.455 0.463)
  '(1033 0.297 0.320 0.339 0.355 0.368 0.379 0.397 0.411 0.421 0.429 0.436 0.446 0.460)
  '(1034 0.289 0.312 0.331 0.346 0.359 0.371 0.388 0.402 0.412 0.420 0.427 0.437 0.452)

  '(1035 0.282 0.304 0.323 0.338 0.351 0.362 0.380 0.393 0.403 0.412 0.418 0.429 0.444)
  '(1036 0.276 0.297 0.315 0.330 0.343 0.354 0.372 0.385 0.395 0.403 0.410 0.421 0.436)
  '(1037 0.269 0.290 0.308 0.323 0.335 0.346 0.364 0.377 0.387 0.395 0.402 0.413 0.429)
  '(1038 0.263 0.284 0.301 0.316 0.328 0.339 0.356 0.369 0.379 0.388 0.394 0.405 0.422)
  '(1039 0.257 0.277 0.294 0.309 0.321 0.332 0.349 0.362 0.372 0.380 0.387 0.398 0.415)

  '(1040 0.251 0.271 0.288 0.302 0.314 0.325 0.342 0.355 0.365 0.373 0.379 0.391 0.408)
  '(1041 0.246 0.265 0.282 0.296 0.308 0.318 0.335 0.348 0.358 0.366 0.372 0.384 0.402)
  '(1042 0.241 0.260 0.276 0.290 0.302 0.312 0.328 0.341 0.352 0.359 0.366 0.377 0.396)
  '(1043 0.236 0.255 0.270 0.284 0.296 0.306 0.322 0.335 0.345 0.353 0.359 0.371 0.390)
  '(1044 0.231 0.250 0.265 0.278 0.290 0.300 0.316 0.329 0.339 0.347 0.353 0.365 0.384)

  '(1045 0.226 0.245 0.260 0.273 0.285 0.294 0.310 0.323 0.333 0.341 0.347 0.359 0.378)
  '(1046 0.222 0.240 0.255 0.268 0.279 0.289 0.305 0.317 0.327 0.335 0.341 0.353 0.373)
  '(1047 0.218 0.235 0.250 0.263 0.274 0.284 0.299 0.312 0.321 0.329 0.336 0.347 0.367)
  '(1048 0.214 0.231 0.245 0.258 0.269 0.279 0.294 0.306 0.316 0.324 0.330 0.342 0.362)
  '(1049 0.210 0.227 0.241 0.252 0.265 0.274 0.289 0.301 0.311 0.319 0.325 0.337 0.357)
  '(1050 0.206 0.223 0.237 0.249 0.260 0.269 0.284 0.296 0.306 0.313 0.320 0.332 0.352)
  '(1051 0.206 0.223 0.237 0.249 0.260 0.269 0.284 0.296 0.306 0.313 0.320 0.332 0.352)
 )  
 ); setq end
;参数输入
 (command "zoom" "e")
 (princ "\n请输入参数表插入点:")  
 (setq bl1 (getvar "userr1") ;获取当前图形比例
       bl1 (/ bl1 100.0)     ;计算图块缩放比例
       lx (* bl1 12042)
       ly (* bl1 7200)
 ) ;setq end
 (command "insert" "border" "x" lx "y" ly "r" 0 pause)
 (command "erase" "l" "")
 (setq p0 (getvar "lastpoint")
       p1 (polar p0 0 lx)
       p1 (polar p1 pi09 ly)
 ) ;setq end
 (command "insert" "地质资料表" p0 bl1 bl1 0)
 (command "zoom" "w" p0 p1)
 (setq scale 1)
 (load "tab.lsp")
 (setq JA (getreal "\n基础长(m):")   ;逐个输入参数并写入表格中
       tx (* bl1 2470)
       ty (* bl1 1000)
       tp (polar p0 0 tx)
       tp (polar tp pi09 ty)
       txt (rtos JA 2 1)
       th (* 3.5 (getvar "userr1"))
 ) ;setq end
 (command "text" tp th 0 txt)
 (setq JB (getreal "基础宽(m):")
       tp (polar tp pi27 (* bl1 800))
       txt (rtos JB 2 1)
 ) ;setq end
 (if (>= JA JB) 
       (setq jn (/ JA JB))
       (setq wap JA ;如果JA<JB,数据互换
             JA JB
             JB wap
             jn (/ JA JB)
       ) ;setq end
 ) ;if end
 (command "text" tp th 0 txt) 
 (setq h (getreal "\n基础埋深(m):")
       tx (* bl1 8950)
       ty (* bl1 6650)
       tp (polar p0 0 tx)
       tp (polar tp pi09 ty)
       txt (rtos h 2 1)
       th (* 3.5 (getvar "userr1"))
 ) ;setq end
 (command "text" tp th 0 txt)
 (setq p (getreal "基底压力(KN/m2):")
       tp (polar tp pi27 (* bl1 800))
       txt (rtos p 2 1)
       th (* 3.5 (getvar "userr1"))
 ) ;setq end
 (command "text" tp th 0 txt)
 (setq sw (getreal "地下水埋深(m):")
       tp (polar tp pi27 (* bl1 800))
       txt (rtos sw 2 1)
       th (* 3.5 (getvar "userr1"))
       temp T
       index 1
 ) ;setq end
 (command "text" tp th 0 txt)
 (setq l-dre '()
       tx (* bl1 6650)
       ty (* bl1 (+ 800 3350))
       tp1 (polar p0 0 tx)
       tp1 (polar tp1 pi09 ty)
 ) ;setq end
 (while temp ;循环输入各土层参数,并写入表格 
  (setq str (strcat "\n输入第" (rtos index 2 0) "层土Di,ri,Esi:"))
  (princ str)
  (setq l-l (readlh))
  (if (= (nth 0 l-l) 0.0)
      (setq temp nil)
      (if (< (length l-l) 3)
         (princ "\n参数输入错，请重新输入!")
         (progn
           (setq l-dre (cons l-l l-dre) 
                 txtd (rtos (nth 0 l-l) 2 1)
                 txtr (rtos (nth 1 l-l) 2 1)
                 txte (rtos (nth 2 l-l) 2 1)
 		 tp1 (polar tp1 pi27 (* 800 bl1))
   		 tp2 (polar tp1 0 (* 1700 bl1))
   		 tp3 (polar tp2 0 (* 1700 bl1))
		 index (1+ index)                
           ) ;setq end
           (command "text" tp1 th 0 txtd)
           (command "text" tp2 th 0 txtr)
           (command "text" tp3 th 0 txte)
        ) ;progn end
      ) ;if end
   ) ;if end
  ) ;while end
  (setq l-dre (reverse l-dre))

;计算p0 
 (command "zoom" "e")
 (princ "\n请输入计算表插入点:")
 (setq bl1 (getvar "userr1")
       bl1 (/ bl1 100.0)
       lx (* bl1 20000)
       ly (* bl1 10000)
 ) ;setq end
 (command "insert" "border" "x" lx "y" ly "r" 0 pause)
 (command "erase" "l" "")
 (setq p0 (getvar "lastpoint")
       p1 (polar p0 0 lx)
       p1 (polar p1 pi09 ly)
       p2 (polar p0 pi27 1000)
 ) ;setq end
 (command "insert" "沉降计算表" p0 bl1 bl1 0)
 (command "zoom" "w" p2 p1)
 (setq DZ '()
       Dzz '(0)
       rz '()
       EsZ '()
       len (length l-dre)
       index 0
       att 0
 ) ;setq end
 (repeat len
  (setq at (nth index l-dre)  ;提取土层参数表(Di ri Esi)
        att (+ att (nth 0 at)) ;计算土层埋深
        Dz (cons (nth 0 at) Dz) ;生成各土层深度表(D1 D2 D3 .....)
        Dzz (cons att Dzz)      ;生成各土层埋深表(0 Z1 Z2 Z3 ....)
        rz (cons (nth 1 at) rz) ;生成各土层容重表(r1 r2 r3 .....)
        Esz (cons (nth 2 at) Esz) ;生成各土层压缩模量表(Es1 Es2 Es3 ....)
        index (1+ index)
  ) ;setq end
 ) ;repeat end
  (setq Dz (reverse Dz)
        Dzz (reverse Dzz)
        rz (reverse rz)
        Esz (reverse Esz)
        jud t
        index 1
        d3 h
        d4 sw 
        pc 0
  ) ;setq end
  (while jud ;计算基底有效土压力
    (setq d2 (nth index Dzz)
          d1 (nth (1- index) Dzz)
          j1 (comp d1 d2 d3) ;判断当前土层与基础埋深的关系
    ) ;setq end
    (if (= j1 2) (setq jud nil d2 h)) ;如果基础地面在计算土层之间，结束循环
    (setq j2 (comp d1 d2 d4)) ;判断当前土层与地下水位的关系
    (if (= j2 1) ;地下水位在土层以上
        (setq rr (nth (1- index) rz)
              pc (+ pc (* (- rr 10) (- d2 d1)))
        ) ;setq end
    ) ;if end
    (if (= j2 2) ;地下水位在土层之间
        (setq rr (nth (1- index) rz)
              hh1 (- sw d1)
              hh2 (- d2 sw)
              pc1 (* rr hh1)
              pc2 (* (- rr 10) hh2)
              pc (+ pc pc1 pc2)
         ) ;setq end
    ) ;if end
    (if (= j2 3) ;地下水位在土层以下
        (setq rr (nth (1- index) rz)
              pc (+ pc (* rr (- d2 d1)))
        ) ;setq end
    ) ;if end
    (setq index (1+ index))
    ;(princ (strcat "\npc=" (rtos pc 2 2)))
  ) ;while end
  (setq lx (* bl1 1730) ly (* bl1 7920)
        tp1 (list (+ lx (nth 0 p0)) (+ ly (nth 1 p0)) )
        tp2 (polar tp1 0 (* bl1 3710))
        tp3 (polar tp2 0 (* bl1 5600))
        tp4 (polar tp3 0 (* bl1 5070))
        txt1 (rtos jn 2 2)
        txt2 (rtos p 2 1)
        txt3 (rtos pc 2 1)
        p00 (- p pc)  ;基底有效压力
        txt4 (rtos p00 2 1)
  ) ;setq end
  (command "text" tp1 th 0 txt1)
  (command "text" tp2 th 0 txt2)
  (command "text" tp3 th 0 txt3)
  (command "text" tp4 th 0 txt4)
          
 (setq l-s1 '(0 0 1.0 0)
       ind1 0 ind2 0
       lx (* bl1 200)
       ly (* bl1 5750)
       tp1 (list (+ lx (nth 0 p0)) (+ ly (nth 1 p0)) )
       sum-s 0
       sum-s1 0	
       temp t
       z0 0 c0 1 zc0 0
 ) ;setq end
 (repeat (length l-s1) ;写入z=0时的数据
   (setq tp (polar tp1 0 (* (* 2500 ind2) bl1))
         txt (rtos (nth ind2 l-s1) 2 3)
         ind2 (1+ ind2)
   ) ;setq end
   (command "text" tp th 0 txt)
 ) ;repeat end
 (setq tp1 (polar tp1 pi27 (* 800 bl1)))
 (while temp ;分层计算沉降
  (setq i (+ index ind1 -1)
        Z1 (- (nth i Dzz) h 1)
        zb1 (/ z1 JB)
        ZZ (- (nth i Dzz) h)
        bb (/ ZZ JB)
  ) ;setq end
  (if (>= bb 5) ;如果z/B>5 结束循环
      (Setq temp nil 
            Z1 (- (* 5 jb) 1)
            zb1 (/ z1 JB)
       ) ;setq end
  ) ;if end
  (if (> z1 0)
   (progn   
    (setq
        c1 (func zb1 jn) ;计算平均附加应力系数
        zc1 (* z1 c1)
	txtt (strcat "\nz1=" (rtos z1 2 2) " c1=" (rtos c1 2 4) " zc1=" (rtos zc1 2 4) "\n")
    ) ;setq end
;    (princ txtt)
    (setq 
        zc2 (- zc1 zc0)
        Es (nth (1- i) Esz)
        dsi (* (/ zc2 Es) p00) ;本土层沉降
        sum-s2 (+ sum-s dsi)   ;累计沉降
        Z2 (+ Z1  1)
        zb2 (/ z2 Jb)  
        c2 (func Zb2 jn)
        zc3 (* z2 c2)
        zc4 (- zc3 zc1)
        dsi1 (* (/ zc4 Es) p00)
        sum-s1 (+ sum-s2 dsi1)
        op (/ dsi1 sum-s1)
    ) ;setq end
 ;  (Princ (strcat "\nzb1=" (rtos zb1 2 4)))
 ;  (Princ (strcat "\nzb2=" (rtos zb2 2 4)))

   (if  (< op 0.025) ;判别是否满足结束计算条件
       (progn 
         (setq l-s1 (list z1 zb1 c1 zc1 zc2 es dsi sum-s2)
	       ind2 0)
         (repeat (length l-s1)
          (setq tp (polar tp1 0 (*  2500 ind2 bl1))
               txt (rtos (nth ind2 l-s1) 2 4)
	       ind2 (1+ ind2)
	  ) ;setq end
	  (command "text" tp th 0 txt) ;写入计算结果
	 ) ;repeat end         
         (setq tp1 (polar tp1 pi27 (* 800 bl1)))
         (setq l-s1 (list z2 zb2 c2 zc3 zc4 es dsi1 sum-s1)
	       ind2 0)
         (repeat (length l-s1)
          (setq tp (polar tp1 0 (* ind2 2500 bl1))
               txt (rtos (nth ind2 l-s1) 2 4)
	       ind2 (1+ ind2)
	  ) ;setq end
	  (command "text" tp th 0 txt)
	 ) ;repeat end         
         (setq  tp1 (polar tp1 pi27 (* 800 bl1)))
         (setq temp nil sum-s sum-s1)
       ) ;progn end
       (progn 
	  (setq Z1 Z2
	        zb1 (/ z1 Jb)
	        c1 (func zb1 jn)
	        zc1 (* z1 c1)
	        zc2 (- zc1 zc0)
	        Es (nth (1- i) Esz)
	        dsi (* (/ zc2 Es) p00)
	        sum-s (+ sum-s dsi)
          ) ;setq end
         (setq l-s1 (list z1 zb1 c1 zc1 zc2 es dsi sum-s)
	       ind2 0)
         (repeat (length l-s1)
          (setq tp (polar tp1 0 (*  ind2 2500 bl1))
               txt (rtos (nth ind2 l-s1) 2 4)
	       ind2 (1+ ind2)
	  ) ;setq end
	  (command "text" tp th 0 txt) ;写入计算结果
	 ) ;repeat end         
         (setq tp1 (polar tp1 pi27 (* 800 bl1)))
   	 (setq Z0 Z1
               c0 c1
               zc0 zc1
               ind1 (1+ ind1)
	 ) ;setq end
      ) ;progn end
     ) ;if end
     (setq temp1 t)
     (if (and temp (>= i (length Dz))) 
         (progn 
            (princ "\n您提供的土层不够，请输入更多的土层参数!")
            (setq temp nil temp1 nil)
         ) ;progn end
     ) ;if end
    ) ;progn end
    (setq ind1 (1+ ind1))
   ) ;if end  
  );while end
(if temp1
 (progn
 (setq sum-es 0 
        ind (1- index)
        sum-h 0
  ) ;setq end
  (while (<= ind i) ;计算沉降计算范围内压缩模量的当量值Es
      (if (= ind (1- index))
	     (setq hi (nth ind Dzz)
                   hi (- hi h)
                   es (nth (1- ind) Esz)
 		   sum-es (+ sum-es (* es hi))
                   sum-h  (+ sum-h hi)
              ) ;setq end
	     (setq hi (nth (1- ind) Dz)
                   es (nth (1- ind) Esz)
 		   sum-es (+ sum-es (* es hi))
                   sum-h  (+ sum-h hi)
              ) ;setq end
      ) ;if end
      (setq ind (1+ ind))
   );while end
   (Setq es (/ sum-es sum-h))
   (cond
     ((if (<= es 4000) (setq ms 1.3)))
     ((if (and (> es 4000) (<= es 7000)) (setq ms 1.0)))
     ((if (and (> es 7000) (<= es 15000)) (setq ms 0.7)))
     ((if (and (> es 15000) (<= es 20000)) (setq ms 0.5)))
     ((if (> es 20000) (setq ms 0.2)))
   ) ;cond end
   (setq s (* ms sum-s)
         tp (list (+ (nth 0 p0) (* 11000 bl1))
                  (- (nth 1 p0) (* 700 bl1)) )
         txt (strcat (rtos ms 2 1) "X" (rtos sum-s 2 4) " = " (rtos s 2 4) " (m)" )
   ) ;setq end
   (command "text" tp th 0 txt)
  );progn end
 ) ;if end
 (resvar) ;还原系统变量
);defun end  