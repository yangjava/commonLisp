(defun C:dimtext( )
(princ "\n选择要修改的尺寸线:")
(setq ss (ssget))
(setq sl (sslength ss))
(setq txt (getstring "\n输入新尺寸值:")) 
(setvar "cmdecho" 0)
(setq index 0)
(repeat sl
    (setq ent (entget (ssname ss index)))
    (setq index (+ 1 index))
    (setq ty (cdr (assoc 0 ent)))
    (if (= "DIMENSION" ty)
        (progn
         (setq oldtxt (assoc 1 ent))
         (setq newtxt (cons (car oldtxt) txt))
         (setq ent1 (subst newtxt oldtxt ent))
         (entmod ent1)
        ) ;progn end
    ) ;repeat end
)     ;while end
(setvar "cmdecho" 1)
)
         
          
