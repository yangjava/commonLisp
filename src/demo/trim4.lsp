;+---------------------+
;+ 四条直线快速修剪    +
;+ 文件名: TRIM4.lsp   +
;+---------------------+
(defun C:TRIM4( / index index1 ss p0 ss1 entn ent ty l_p1 l_p2 p1 p2 p3 p4 ang1 ang2 
                  ang3 dist1 dist2 )
 (inivar) ;初始化系统变量
 (setvar "blipmode" 1)
 (setq index 1 
       ss (ssadd) ;构件一个空选择集
 ) ;setq end
 (setvar "osmode" 512) ;设定捕捉模式为Nearest
 (while (<= index 4)
   (princ (strcat "\n选取第" (rtos index 2 0) "根线:"))
   (setq p0 (getpoint)
         ss1 (ssget p0) ;获取通过该点的实体
         entn (ssname ss1 0)
         ent (entget entn)
         ty (cdr (assoc 0 ent))
   ) ;setq end
   (if (= ty "LINE")
       (setq index (1+ index)
             ss (ssadd entn ss) ;把实体加入选择集
       ) ;setq end
       (princ "\n对不起!选择错误,请重新选择")
   ) ;if end
  ) ;while end
 (setvar "osmode" 0)
 (setvar "blipmode" 0)
 (setq index 0
       l_p1 '()
       l_p2 '()
  ) ;setq end
 (repeat 4 ;获取每一根线的起点、终点
  (setq ent (entget (ssname ss index))
        index (+ 1 index)
	p1 (cdr (assoc 10 ent))
	p2 (cdr (assoc 11 ent))
	l_p1 (cons p1 l_p1) 
	l_p2 (cons p2 l_p2)
	) ;setq end
 ) ;repeat end
(setq index 0 n 4
       l_int '()
 )
 (repeat (1- n) ;计算交点
	(setq p1  (nth index l_p1)
	      p2  (nth index l_p2) 
	      index1 (1+ index)
	      index (1+ index)
	) ;setq end
	(repeat (- n index)
	 (setq p3 (nth index1 l_p1) 
	       p4 (nth index1 l_p2)
	       index1 (1+ index1)
	       pt (inters p1 p2 p3 p4) ;求交点
	       l_int (if pt (cons pt l_int) l_int)
	 ) ;setq end
	)  ;repeat end
 ) ;repeat end
 (setq index 0 
       la (cdr (assoc 8 ent))
 ) ;setq end
 (repeat n ;删除原直线
  (setq entn (ssname ss index)
	index (1+ index)
  ) ;setq end
  (entdel entn)
 ) ;repeat end
 (setq index 0)
 (command "layer" "s" la "")
 (repeat (1- n) ;重新画线
    (setq p3 (nth index l_int)
	  index2 (1+ index)
	  index (1+ index)
    ) ;setq end
    (repeat (- n index)
	(setq index1 0
	      p4 (nth index2 l_int)
	      index2 (1+ index2)
	) ;setq end
	(repeat n
	 (setq p1  (nth index1 l_p1)
	       p2  (nth index1 l_p2) 
	       index1 (1+ index1)
	 ) ;setq end
	 (setq ang1 (angtos (angle p1 p2) 0 1)
	       ang2 (angtos (angle p1 p3) 0 1)
	       ang3 (angtos (angle p1 p4) 0 1)
	 ) ;setq end
	 (if (= ang1 ang2 ang3)
	     (progn
		 (setq dist1 (distance p1 p3)
		       dist2 (distance p1 p4)
		) ;set end
		(if (< dist1 dist2)
		  (progn
		   (command "line" p1 p3 "")
		   (command "line" p4 p2 "")
		  ) ;progn end
		  (progn
		   (command "line" p1 p4 "")
		   (command "line" p3 p2 "")
		  ) ;progn end
		) ;if end
	     ) ;progn end
	 )  ;if end
	) ;repeat end
    ) ;repeat end
 ) ;repeat end
 (redraw)
 (resvar) ;还原系统变量
 (princ)
) ;defun end
