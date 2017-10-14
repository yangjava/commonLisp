(defun C:dimtext1( )
(inivar) ;初始化系统变量
(setq dim-txt1 (getstring "\n修改前尺寸:")
      dim-txt2 (getstring "\n修改后尺寸:")
) ;setq end
(princ "\n选择要修改的尺寸线:")
(setq ss (ssget))
(setq sl (sslength ss))
(setq index 0)
(repeat sl
    (setq ent (entget (ssname ss index)))
    (setq index (+ 1 index))
    (setq ty (cdr (assoc 0 ent)))
    (if (= "DIMENSION" ty)
       (progn
        (setq oldtxt (assoc 1 ent)
              dim-old (cdr oldtxt)
        ) ;setq end
        (if (= dim-old dim-txt1) 
         (progn
          (setq newtxt (cons (car oldtxt) dim-txt2))
          (setq ent1 (subst newtxt oldtxt ent))
          (entmod ent1)
         ) ;progn end
        ) ;if end 
       ) ;progn end
    ) ;repeat end
)     ;while end
 (resvar) ;还原系统变量
 (princ)
)
         
          
