;+-------------------------------------+
;+将LINE实体变为指定宽度的多义线       +
;+文件名 CHWID2.lsp                    +
;+-------------------------------------+
(defun C:chwid2(/ ss txsize n index ent ty oldsize newsize ent1)
 (inivar) ;初始化系统变量
 (setq ss (ssget)
       wid (getdist "\n输入新的多义线宽度:")
       n (sslength ss)
       index 0
 ) ;setq end
 (repeat n
  (setq entname (ssname ss index) ;取实体名
        ent (entget entname)
        index (+ 1 index)
        ty (assoc 0 ent) ;取实体类型
  ) ;setq end
  (if (= "LINE" (cdr ty))
   (progn
    (setq p1 (cdr (assoc 10 ent))
          p2 (cdr (assoc 11 ent))
          la (cdr (assoc 8 ent))  )
    (entdel entname)
    (command "layer" "s" la "")
    (command "pline" p1 "w" wid wid p2 "")
   ) ;progn end
  )  ;if end
 )  ;repeat end
 (resvar) ;还原系统变量
 (princ)
) ;defun end
