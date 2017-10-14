(defun C:chla(/ ss txsize n index ent ty oldsize newsize ent1)
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
  (if (= "CIRCLE" (cdr ty))
   (progn
    (setq R (cdr (assoc 40 ent))
          cen (cdr (assoc 10 ent))
          p1   (polar cen 0 (+ R 5) )
          p1   (polar p1  pi09 (+ R 5))
          p2   (polar cen pi (+ R 5))
          p2   (polar p2 pi27 (+ R 5))
    ) ;set end
    (command "change" "w"  p1 p2 "" "p" "la" nla "")
   )
  ) ;if end
;  (if (or (= "LINE" (cdr ty)) (= "POLYLINE" (cdr ty)))
;      (entdel name) 
;  ) ;if end
 )
 ;(redraw)
(setvar "cmdecho" 1)
) 
