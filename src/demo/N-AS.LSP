;+-----------------------+
;+ 轴心受压承载力计算    +
;+ 文件名N-AS.lsp        +
;+-----------------------+
;稳定系数查表
(defun ffai(rt mode / temp ind l_sub at l_sub1 l_sub2 at1 at2 ind1 ind2 
                      fai1 fai2 delta dt r_fai)
  (setq l_fai (list  ;稳定系数表格,新规范表7.3.1
      '(1.00 8  7    28)
      '(0.98 10 8.5  35)
      '(0.95 12 10.5 42)
      '(0.92 14 12   48)
      '(0.87 16 14   55)
      '(0.81 18 15.5 62)
      '(0.75 20 17   69)
      '(0.70 22 19   76)
      '(0.65 24 21   83)
      '(0.60 26 22.5 90)
      '(0.56 28 24   97)

      '(0.52 30 26   104)
      '(0.48 32 28   111)
      '(0.44 34 29.5 118)
      '(0.40 36 31   125)
      '(0.36 38 33   132)
      '(0.32 40 34.5 139)
      '(0.29 42 36.5 146)
      '(0.26 44 38   153)
      '(0.23 46 40   160)
      '(0.21 48 41.5 167)
      '(0.19 50 43   174)
     ) ;list end
  ) ;setq end
  (setq temp T
        ind 0
  ) ;setq end
  (while (and temp (< ind 22))
     (setq l_sub (nth ind l_fai)
           at (nth mode l_sub)
     ) ;setq end
     (if (> at rt)
         (setq temp nil)
         (setq ind (1+ ind))
     ) ;if end
  ) ;while end
  (setq ind2 ind
        ind1 (if (= ind 0) ind2 (1- ind2))
        l_sub1 (nth ind1 l_fai)
        l_sub2 (nth ind2 l_fai)
        at1 (nth mode l_sub1)
        at2 (nth mode l_sub2)
        fai1 (nth 0 l_sub1)
        fai2 (nth 0 l_sub2)
        delta (if (> at2 at1)
                  (/ (- fai2 fai1) (- at2 at1))
                  0
              )
        dt  (- rt at1)
  ) ;setq end
  (setq r_fai (+ fai1 (* delta dt))) ;返回插值结果
) ;defun end
;
(defun c:N-as( / l-l len Dia iii b h l0 c fc fy N A rt As txt0 txt1 txt2 Nc)
 (inivar) ;初始化系统变量
 (textscr)
 (load "concrete.lsp")
 (load "tab.lsp")
 (setq scale 1 temp T)
 (while temp
  (princ "\n轴心受压承载力计算:")
  (princ "\n请输入以下数据，如为圆形截面,h输0,如b,h输400,0表示直径400的圆")
  (princ "\n如输入截面最小的回转半径,b用负数输入,h输0,如b,h输-300,0")
  (princ "\nb,h--截面宽，高；l0--构件计算长度；c--砼标号；")
  (princ "\nfy--纵筋等级; As'--全部纵向钢筋面积;N--截面轴向压力设计值 ")
  (princ "\n请输入b(mm),h(mm),l0(m),c,fy,N(KN)")
  (princ "\n:")
  (setq l-l (readlh)
        len (length l-l)
  ) ;setq end
  (if (< len 6) 
     (princ "\n参数输入错，请重新输入：")
     (progn ;progn0
      (setq temp nil
            Dia 0   ;截面直径
            iii 0   ;截面最小的回转半径
            b (nth 0 l-l)
	    h (nth 1 l-l)
            l0 (nth 2 l-l)
            l0 (* l0 1000)
	    c (nth 3 l-l)
  	    fc (concrete "fc" c 1) ;按新规范计算
            Fy (nth 4 l-l)
            Fy (if (= Fy 1) 210.0 310.0)
            N (nth 5 l-l)
            N (* N 1000)
      ) ;setq end
      (if (> h 0)  ;if1
          (setq A  (* b h) ;矩形截面
                rt (/ l0 b 1.0)
                txt0 (strcat "\nl0/b= " (rtos rt 2 4))
                mode 1
          ) ;setq end
          (if (> b 0) ;if2
              (setq dia b  ;圆形截面
                    A   (* pi dia dia 0.25)
                    rt (/ l0 dia)
                    txt0 (strcat "\nl0/d= " (rtos rt 2 4))
                    mode 2
              ) ;setq end
              (setq iii (abs b)  ;截面最小的回转半径
                    A   (getreal "\n截面面积(mm2):")
                    rt (/ l0 iii)
                    txt0 (strcat "\nl0/i= " (rtos rt 2 4))
                    mode 3
              ) ;setq end
          ) ;if2 end
      ) ;if1 end
      (setq fai (ffai rt mode)
            txt1 (strcat "\n稳定系数fai= " (rtos fai 2 4))
            N1 (/ N 0.9 fai)
            NC (* fc A)
            As (/ (- N1 Nc) Fy 1.0)
            txt2 (if (> As 0)
                   (strcat "\nAs'=(N/(0.9*fai)-fc*A)/fy=\n("
                           (rtos N1 2 2) "-" (rtos NC 2 2) ")/" (rtos fy 2 1) "="
                           (rtos As 2 2) " mm2")
                   (strcat "\n计算不需配筋.")
                 ) ;if end
      ) ;setq end
      (princ txt0)
      (princ (strcat "\nA=" (rtos A 2 2) " mm2"))
      (princ txt1)
      (princ txt2)
     ) ;progn0 end
    ) ;if (< len 8) end
   ) ;while temp end
  (resvar) ;还原系统变量
  (princ)
);defun end
