(defun C:chwid(/ ss newwid n index ent ty)
 (inivar) ;初始化系统变量
 (setq ss (ssget))
 (if ss
  (progn
    (setq newwid (getreal "\n输入新多义线宽度:") 
          n (sslength ss) 
          index 0
    ) ;setq end
    (repeat n
     (setq name (ssname ss index)
           ent (entget name)
           index (+ 1 index) 
           ty (assoc 0 ent)
     ) ;setq end
     (if (= "LWPOLYLINE" (cdr ty))
       (command "pedit" name "w" newwid "")
     ) ;if end
    ) ;repeat end
   ) ;progn end
  ) ;if end
 (resvar) ;还原系统变量
 (princ)
) ;defun end
