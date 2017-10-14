;(defun C:del1(/ ss txsize n index ent type oldsize newsize ent1)
 (setq ss (ssget))
 (setvar "cmdecho" 0)
 (setq n (sslength ss))
 (setq index 0)
 (repeat n
  (setq entn (ssname ss index)
       ent (entget entn)
  ) ;setq end
  (setq index (+ 1 index))
  (setq type (cdr (assoc 0 ent))
	la (cdr (assoc 8 ent))        
  ) ;setq end
  ;(if (OR (= "POLYLINE" type) (= "REI" la))
   (if (= "TEXT" type)
    (entdel entn)
  ) ;IF END
 )REPEAT END
(setvar "cmdecho" 1)
 ;(redraw)
;) 
