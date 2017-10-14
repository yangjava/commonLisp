(defun C:chcir(/ ss txsize n index ent ty oldsize newsize ent1)
 (setq ss (ssget))
 (setq txsize (getreal "\nÊäÈëĞÂµÄÔ²°ë¾¶:"))
 (setvar "cmdecho" 0)
 (setq n (sslength ss))
 (setq index 0)
 (repeat n
  (setq ent (entget (ssname ss index)))
  (setq index (+ 1 index))
  (setq ty (assoc 0 ent))
  (if (OR 
          (= "CIRCLE" (cdr ty))
          (= "ARC" (cdr ty))
      ) ;or end
   (progn
    (setq oldsize (assoc 40 ent))
    (setq newsize (cons (car oldsize) txsize))
    (setq ent1 (subst newsize oldsize ent))
    (entmod ent1)
   )
  )
 )
; (redraw)
(setvar "cmdecho" 1)
) 
