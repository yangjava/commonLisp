(command "layer" "m" "hf" "c" "yellow" "" "s" "0" "")
(defun C:HF()
(setvar "cmdecho" 0)
 (command "layer" "s" "hf" "")
 (setq oer *error* *error* clmerr)
(while (setq p1 (getpoint "\n焊缝第一点:"))
 (setq p2 (getpoint "\n焊缝第二点:")
       ang (angle p1 p2)
       dist (distance p1 p2)
       scale (getvar "userr1")
       delta (* 2.0 scale)
       n ( 1+ (fix (/ dist delta)))
       m 0
 ) ;setq end       
 (repeat n
  (setq d (* m delta)
        m (1+ m)
        ph1 (polar p1 ang d)
        ph2 (polar ph1 (+ ang pi09) delta)
  ) ;setq end
  (command "line" ph1 ph2 "")
 ) ;repeat end
) ;while end
(command "layer"  "s" "0" "")
(setvar "cmdecho" 1)
) ;defun end
