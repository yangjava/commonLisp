;»­Æ½Ãæµ²ÍÁÇ½Í¼Àý
(defun C:dtq( )
 (setvar "cmdecho" 0)
 (setvar "osmode" 32)
 (setq p1 (getpoint "\np1:")
       p2 (getpoint "\np2:")
       dist (distance p1 p2)
       ang (angle p1 p2)
       ang1 (+ ang pi09)
       d1 (getvar "userr1")
       nT (fix (/ dist d1))
       ind 1
 ) ;setq end
 (repeat nT
    (setq pp1 (polar p1 ang (* ind d1))
          n1 (/ ind 2)
          n2 (/ ind 2.0)
          d2 (if (= n1 n2) d1 (* 2 d1))
          pp2 (polar pp1 ang1 d2)
          ind (1+ ind)
    ) ;setq end
 (setvar "osmode" 0)
 (command "line" pp1 pp2 "")
 ) ;repeat end
 (setvar "cmdecho" 1)
) ;defun end
