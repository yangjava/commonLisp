(defun C:chla1(/ ss txsize n index ent ty oldsize newsize ent1)
 (setq nla (getstring "\nÐÂ²ãÃû:"))
 (setq ss (ssget))
 (setvar "cmdecho" 0)
 (setq n (sslength ss))
 (setq index 0)
 (repeat n
  (setq name (ssname ss index)
        ent (entget name )
  ) ;setq end
  (setq index (+ 1 index))
  (setq ty (assoc 0 ent))
  (if (= "TEXT" (cdr ty))
   (progn
    (setq txt (cdr (assoc 1 ent))
          t1  (substr txt 1 1)
          t2  (substr txt 2 1)
    )
    (if (or (= t1 "t") (= t2 "L"))
     (command "change" name "" "p" "la" nla "")
    ) ;if end
   )
  ) ;if end
  (if (= "LINE" (cdr ty))
    (command "change" name "" "p" "la" nla "")
  ) ;if end
 )
 ;(redraw)
(setvar "cmdecho" 1)
) 
