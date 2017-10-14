(defun iiif(text txt1 txt2)
 (setq  num  1)
  (while (and (< num (strlen text))
              (not (= (substr text num 1) txt1)) 
              (not (= (substr text num 1) txt2))
         ) ;and end
      (setq num (+ num 1)) ;setq end   
  ) ;while end
  (if (< num (strlen text)) T nil)
) ;defun end


(defun C:chtxt(/ ss n index ent ty oldsize newsize ent1)
 (setq ss (ssget))
 (setq n (sslength ss))
 (setq index 0)
 (setvar "cmdecho" 0)
 (repeat n
  (setq ent (entget (ssname ss index)))
  (setq index (+ 1 index))
  (setq ty (assoc 0 ent))
  (if (and (OR (= "TEXT" (cdr ty)) 
               (= "MTEXT" (CDR TY)) 
           ) ;or end
           (iiif (cdr (assoc 1  ent)) "X" "L")
      );and end
   (progn
    (setq oldsize (assoc 7 ent)
          newsize (cons (car oldsize) "rmc")
          ent1 (subst newsize oldsize ent)
          oldsize (assoc 72 ent1)
          newsize (cons (car oldsize) '0)
          ent2 (subst newsize oldsize ent1)
     ) ;setq end
    (entmod ent2)
   )
  )
 )
 (setvar "cmdecho" 1)
) 
