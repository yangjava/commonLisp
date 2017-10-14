(defun C:exptxt( )
 (setvar "CMDECHO" 0)
 (setq rad (getreal "\n输入圆弧半径:"))
 (princ "\n输入要炸开的文本:")
 (setq ss (ssget))
 (setq n (sslength ss))
 (setq index 0)
 (repeat n
  (setq ent (entget (ssname ss index)))
  (setq type (assoc 0 ent))
  (if (= "TEXT" (cdr type))
   (progn
    (setq h0    (cdr (assoc 40 ent))
          text  (cdr (assoc 1  ent))
          insp  (cdr (assoc 10 ent))
          ang   (cdr (assoc 50 ent))
          scale (cdr (assoc 41 ent))
          arc_cen (polar insp (+ (* ang pi (/ 1 180)) pi09) rad)
          alfa1  (/ (* h0 scale) r)
    ) ;setq end
    (setq x0  (nth 0 insp)
          y0  (nth 1 insp)
          nn  (strlen text)
    ) ;setq end
    (setq h h0)
    (setq sca (* h scale))
    (setq x0 (- x0 (* sca (cos ang))))
    (setq y0 (- y0 (* sca (sin ang))))
    (entdel (ssname ss index))
    (setq ind 1)
;   (setq ind1 0)
    (while (<= ind nn)
;
      (setq txt (substr text ind 1))
      (setq h h0)
      (setq sca (* h scale))
      (setq nasc (ascii txt))
      (if (> nasc 134)
        (progn
          (setq h (* h0 (/ 15.0 15.0)))
          (setq sca (* h scale))
          (setq txt (substr text ind 2))
          (setq ind (+ ind 2))
        ) ;progn end
       (setq ind (+ ind 1))
      )   ;if end   
;      
;     (setq ind1 (+ 1 ind1))
      (setq x (+ x0 (* sca (cos ang))))
      (setq y (+ y0 (* sca (sin ang))))
      (setq p (list x y))
      (setq ang1 (* 180 (/ ang pi)))
      (command "text" p h ang1 txt)
      (setq x0 x
            y0 y)
    ) ;while end
   )  ;progn end
  )   ;if end
  (setq index (+ 1 index))
 )    ;repeat2 end   
(command "redraw")
(setvar "CMDECHO" 1)
)
