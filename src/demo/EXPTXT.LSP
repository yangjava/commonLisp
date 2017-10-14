;+---------------------------------------------+
;+ 字符串的炸开,并调整字母与汉字之间的高度差   +
;+  文件名: EXPTXT.lsp                         +
;+---------------------------------------------+
(defun C:exptxt( )
 (inivar) ;初始化系统变量
 (setvar "cmdecho" 1)
 (princ "\n选择要炸开的文本:")
 (setq ss (ssget)
       mode (getint "\n0.文字高度不变 1.以汉字高度为准 2.以数字或大写字母高度为准 <0>:")
       mode (if mode mode 0)
       h_asc (if (= mode 1) 0.8 1) ;如以汉字高度为准,字母高度缩小0.8倍
       h_hz  (if (= mode 2) 1.25 1);如以字母高度为准,汉字高度放大1.25倍
       n (sslength ss)
       index 0
 ) ;if end
 (repeat n
  (setq ent (entget (ssname ss index)))
  (setq ty (assoc 0 ent))
  (if (= "TEXT" (cdr ty))
   (progn ;progn1
    (setq h0    (cdr (assoc 40 ent)) ;取字高
          text  (cdr (assoc 1  ent)) ;取文本
          insp  (cdr (assoc 10 ent)) ;取起点
          ang   (cdr (assoc 50 ent)) ;取角度
          scale (cdr (assoc 41 ent)) ;取高宽比
          sca (* h0 scale)
    ) ;setq end
    (setq x0  (nth 0 insp)
          y0  (nth 1 insp)
          nn  (strlen text)
          x0 (- x0 (* sca (cos ang)))
          y0 (- y0 (* sca (sin ang)))
    ) ;setq end
    (entdel (ssname ss index)) ;删除字符实体
    (setq ind 1)
    (while (<= ind nn)
      (setq txt (substr text ind 1)
            h (* h0 h_asc)
            sca (* h scale)
            nasc (ascii txt)
      ) ;setq end
       (if (> nasc 134)
          (setq h (* h0 h_hz)
               sca (* h scale)
               txt (substr text ind 2)
               ind (+ ind 2)
          ) ;setq end
          (setq ind (+ ind 1))
      )   ;if end   
      (setq x (+ x0 (* sca (cos ang))))
      (setq y (+ y0 (* sca (sin ang))))
      (setq p (list x y))
      (setq ang1 (* 180 (/ ang pi)))
      (command "text" p h ang1 txt)
      (setq x0 x y0 y)
    ) ;while end
   )  ;progn1 end
  )   ;if end
  (setq index (+ 1 index))
 )    ;repeat2 end   
 (resvar) ;还原系统变量
 (princ)
) ;defun end
