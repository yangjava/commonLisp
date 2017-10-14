;+------------------------+
;+ 矩形截面受拉承载力计算 +
;+ 文件名N1-AS.lsp         +
;+------------------------+
;
(defun c:N1-as( / l-l len b h h0 as fy N M MM As txt e0 ee)
 (inivar) ;初始化系统变量
 (textscr)
 (load "concrete.lsp")
 (load "tab.lsp")
 (setq scale 1 temp T)
 (while temp
  (princ "\n矩形截面受拉承载力计算:")
  (princ "\n请输入以下数据，如弯矩M没有输0")
  (princ "\nb,h--截面宽，高；fy--纵筋等级; N,M--截面轴向压力,弯矩设计值 ")
  (princ "\n请输入b(mm),h(mm),fy,N(KN),M(KN-m)")
  (princ "\n:")
  (setq l-l (readlh)
        len (length l-l)
  ) ;setq end
  (if (< len 5) 
     (princ "\n参数输入错，请重新输入：")
     (progn ;progn0
      (setq temp nil
            b (nth 0 l-l)
	    h (nth 1 l-l)
            h0 (- h 35)
            as 35
            Fy (nth 2 l-l)
            Fy (if (= Fy 1) 210.0 310.0)
            N (nth 3 l-l)
            N (* N 1000)
            M (nth 4 l-l)
            M (* M 1e6)
      ) ;setq end
      (if (= M 0) ;if1 
       (progn ;轴心受拉
          (princ "\n轴心受拉")
          (setq As (/ N fy 1.0)
                txt (strcat "\n全截面钢筋 As=N/fy=" (rtos As 2 2) " mm2")
          ) ;setq end
          (princ txt)
       ) ;progn end
       (progn ;偏心受拉
          (princ "\n偏心受拉")
          (setq e0 (/ M N 1.0)
                ee (+ (* 0.5 h) e0 -35)
                MM (* N ee)
                As (/ MM fy (- h0 as) 1.0)
                txt (strcat 
                    "\ne0=M/N=" (rtos e0 2 4) " mm"
                    "\ne'=h/2-as+e0=" (rtos ee 2 2) " mm"
                    "\n上下对称钢筋 As=Ne'/fy/(h0-as)=" (rtos As 2 2) " mm2")
          ) ;setq end
          (princ txt)
       ) ;progn end
      ) ;if1 end
     ) ;progn0 end
    ) ;if (< len 5) end
   ) ;while temp end
  (resvar) ;还原系统变量
  (princ)
);defun end
