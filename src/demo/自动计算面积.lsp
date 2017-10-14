;get area
(defun C:GETAREA  ( /  ss e ent lyname ls llist)
  (setq ss (ssget))
  (while ss
    (setq 
      e (ssname ss 0) 
      ent (entget e)
      ss (if (> (sslength ss) 1)
             (ssdel e ss)
          nil
         )
    )
    (cond ((or (=  (cdr (assoc 0 ent))  "LWPOLYLINE") (=  (cdr (assoc 0 ent))  "POLYLINE"))
      (command "AREA" "e" e)
      (setq
        lyname (cdr (assoc 8 ent))
        ls (assoc lyname llist)
        ls1 (cons lyname (+ (if ls (cdr ls) 0) (getvar "AREA"))) 
        llist (if ls (subst ls1 ls llist) (cons ls1 llist))
      )
    ))
  )
  llist
)


      
  

