;+-----------------------+
;+ 矩形截面扭曲承载力计算 +
;+ 文件名VT-AS.lsp        +
;+-----------------------+
;计算混凝土强度影响系数
(defun betac(n / Bc)
 (cond
   ((<= n 50) (setq Bc 1.0))
   ((= n 80)  (setq Bc 0.8))
   (T (setq Bc (- 1.0 (* (/ 0.2 30) (- n 50))))) ;插值
 ) ;cond end
) ;defun end

(defun c:VT-as( / b h h0 V TT fc ft len aa1 aa2 aa3 Acor Ucor txt1 txt2 txt3 Vt1 Vt2 bb1 bb2)
 (inivar) ;初始化系统变量
 (textscr)
 (load "concrete.lsp")
 (load "tab.lsp")
 (setq scale 1 temp T)
 (while temp
  (princ "\n矩形截面扭曲承载力计算:")
  (princ "\n请输入以下数据，如lamda、V没有，可输入0")
  (princ "\nb,h--截面宽，高；V,T--设计剪力值,扭矩；")
  (princ "\nc--砼标号；lamda-- 剪跨比")
  (princ "\nfyv--箍筋等级；fy--纵筋等级")
  (princ "\n请输入b(mm),h(mm),c,lamda,V(KN),T(KN),fyv,fy")
  (princ "\n:")
  (setq l-l (readlh)
        len (length l-l)
  ) ;setq end
  (if (< len 8) 
     (princ "\n参数输入错，请重新输入：")
     (progn ;progn0
      (setq temp nil
            b (nth 0 l-l)
	    h (nth 1 l-l)
	    h0 (- h 35)
            Acor (* (- b 50) (- h 50))
            Ucor (* 2 (+ (- b 50) (- h 50)))
            txt1 (strcat "\nAcor= " (rtos Acor 2 2) " mm2")
            txt2 (strcat "\nUcor= " (rtos Ucor 2 2) " mm")
	    c (nth 2 l-l)
  	    ft (concrete "ft" c 1) ;按新规范计算
  	    fc (concrete "fc" c 1) ;按新规范计算
	    lamda (nth 3 l-l)
            lamda (cond
                     ((and (< lamda 1.5) (> lamda 0.01)) 1.5)
                     ((> lamda 3.0) 3.0)
                     (T lamda)
                  ) ;cond end
	    V (nth 4 l-l)
            V (* V 1e3) ;单位转换
	    TT (nth 5 l-l)
            TT (* TT 1e6) ;单位转换
            Fyv (nth 6 l-l)
            Fy  (nth 7 l-l)
            fyv (if (= fyv 1) 210.0 310.0) ;钢筋强度
            fy  (if (= fy 1) 210.0 310.0)   ;钢筋强度
            Bc (betac c)
            Wt (* b b (- (* 3 h) b) (/ 1.0 6.0))
            txt3 (strcat "\n截面受扭塑性抵抗矩Wt=" (rtos Wt 2 2) " mm3")
            aa1 (* 0.25 Bc fc) ;当h/b<=4
            aa2 (* 0.20 Bc fc) ;当h/b=6
            hb (/ h b 1.0)
            aa3 (cond
                  ( (<= bh 4.0) aa1)
                  ( (= bh 6.0) aa2)
                  (T (+ aa1 (* (- aa2 aa1) (- hb 4.0) 0.5)
                     )
                  )
                ) ;cond end
            VT1 (+ (/ V b h0 1.0) (/ TT 0.8 Wt 1.0))
            VT2 (+ (/ V b h0 1.0) (/ TT Wt 1.0))
        ) ;setq end
        (princ txt1)
        (princ txt2)
        (princ txt3)
        (if (> VT1 aa3)
            (princ "\n截面不够,请加大截面!")
            (if (<= VT2 (* 0.7 ft))
                (princ "\n计算不需配抗扭钢筋,请按构造配筋!")
                (princ "\n受扭构件纵向钢筋与箍筋的配筋强度比值ksi=1.2")
            ) ;if end
        ) ;if end
        (if (and (<= VT1 aa3) (> VT2 (* 0.7 ft)))  ;if1
         (if (= V 0) ;纯扭构件 if2
           (progn
               (setq ksi 1.2
                     bb1 (- TT (* 0.35 ft Wt))
                     bb2 (* 1.2 fyv Acor (sqrt ksi))
                     Ast1 (/ bb1 bb2)
                     txt1 (strcat "\n箍筋 Ast1/s=" (rtos Ast1 2 4) " mm2/mm")
               );setq end
               (princ "\n纯扭构件")
               (princ txt1)
               (setq s (getreal "\n箍筋间距:")
                     Ast2 (* Ast1 s)
                     txt1 (strcat "\n箍筋 Ast1=" (rtos Ast2 2 4) " mm2")
                     Astl (* Ksi fyv Ast1 Ucor (/ 1.0 fy))
                     txt2 (strcat "\n纵筋Astl=" (rtos Astl 2 4) " mm2")
               );setq end
               (princ txt1)
               (princ txt2)
           ) ;progn 纯扭结束
           (progn  ;剪扭构件
            (if (= lamda 0)   ;if3 一般剪扭构件
               (progn
                (setq aa1 (* 0.5 V Wt)
                      aa1 (/ aa1 TT b h0)
                      betat (/ 1.5 (+ 1.0 aa1))
                      betat (cond
                             ( (< betat 0.5) 0.5)
                             ( (> betat 1.0) 1.0)
                             ( T betat)
                            ) ;cond end
                      txt1 (strcat "\nBt= " (rtos betat 2 4))
                      aa2 (* 0.7 ft b h0 (- 1.5 betat))
                      aa2 (- V aa2)
                      Asvs (/ aa2 1.25 fyv h0)
                      txt2 (strcat "\nAsv/s= " (rtos Asvs 2 4) " mm2/mm")
                ) ;setq end
                (princ "\n一般剪扭构件")
                (princ txt1)
                (princ txt2)
                (setq s (getreal "\n箍筋间距:")
                      Asv (* Asvs s)
                      txt1 (strcat "\nAsv= " (rtos Asv 2 4) " mm2")
                ) ;setq end
                (princ txt1)
               ) ;progn end  一般剪扭构件
               (progn    ;集中荷载作用下剪扭构件
                (setq aa1 (* 0.2 V Wt (+ lamda 1.0))
                      aa1 (/ aa1 TT b h0)
                      betat (/ 1.5 (+ 1.0 aa1))
                      betat (cond
                             ( (< betat 0.5) 0.5)
                             ( (> betat 1.0) 1.0)
                             ( T betat)
                            ) ;cond end
                      txt1 (strcat "\nBt= " (rtos betat 2 4))
                      aa2 (* 1.75 ft b h0 (- 1.5 betat) 
                            (/ 1.0 (+ lamda 1.0))
                          )
                      aa2 (- V aa2)
                      Asvs (/ aa2 fyv h0)
                      txt2 (strcat "\nAsv/s= " (rtos Asvs 2 4) " mm2/mm")
                ) ;setq end
                (princ "\n集中荷载作用下剪扭构件")
                (princ txt1)
                (princ txt2)
                (setq s (getreal "\n箍筋间距:")
                      Asv (* Asvs s)
                      txt1 (strcat "\nAsv= " (rtos Asv 2 4) " mm2")
                ) ;setq end
                (princ txt1)
               ) ;progn end
            ) ;if3 end
            ;受扭承载力计算
            (setq ksi 1.2
                  bb1 (- TT (* 0.35 ft Wt betat))
                  bb2 (* 1.2 fyv Acor (sqrt ksi))
                  Ast1 (/ bb1 bb2)
                  txt1 (strcat "\n箍筋 Ast1/s=" (rtos Ast1 2 4) " mm2/mm")
            );setq end
            (princ txt1)
            (setq Ast2 (* Ast1 s)
                  txt1 (strcat "\n箍筋 Ast1=" (rtos Ast2 2 4) " mm2")
                  Astl (* Ksi fyv Ast1 Ucor (/ 1.0 fy))
                  txt2 (strcat "\n纵筋Astl=" (rtos Astl 2 4) " mm2")
            );setq end
            (princ txt1)
            (princ txt2)
           ) ;progn end 剪扭构件结束
          ) ;if2 end
         ) ;if1 end          
      ) ;progn0 end
     ) ;if (< len 8) end
    ) ;while temp end
  (resvar) ;还原系统变量
  (princ)
);defun end
