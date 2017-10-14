(defun C:del1(/ ss txsize n index ent type oldsize newsize ent1)
 (setq ss (ssget))
 (setvar "cmdecho" 0)
 (setq n (sslength ss))
 (setq index 0)
 (repeat n
  (setq ent (entget (ssname ss index)))
  (setq index (+ 1 index))
  (setq type (assoc 0 ent))
  (if (= "CIRCLE" (cdr type))
   (progn
     (setq cen (cdr (assoc 10 ent))
	   r (cdr (assoc 40 ent))
	   scale (getvar "userr1")
	   d (+ r (* 3 scale))
	   p1 (polar cen pi18 d)
	   p1 (polar p1 pi27 d)
	   p2 (polar cen 0 d)
	   p2 (polar p2 pi09 d)
	   ss1 (ssget "w" p1 p2)
	   nle1 (sslength ss1)
	   in 0
      )
      (repeat nle1
	   (setq ent1 (entget (ssname ss1 in))
		 ty (cdr (assoc 0 ent1))
	   ) ;setq end
;          (princ "\ntype:") (princ ty)
	   (if (= "CIRCLE" ty)
	       (entdel (ssname ss1 in))
	   ) ;if end
	   (if (= ty "LINE")
	      (progn   
		(setq p1 (cdr (assoc 10 ent1))
		      p2 (cdr (assoc 11 ent1))
		      ang1 (angtos (angle p1 p2) 0 1)
		      ang2 (angtos (angle p1 cen) 0 1)
		) ;setq end
		(if (= ang1 ang2)
		 (entdel (ssname ss1 in))
		) ;if end
	      ) ;progn end
	   ) ;if end
	   (setq in (1+ in))
       ) ;repeat end
   ) ;PROGN END
  ) ;IF END
 )REPEAT END
(setvar "cmdecho" 1)
 ;(redraw)
) 
