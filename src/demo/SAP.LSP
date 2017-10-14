(defun tran(str / n in l_l at)
 (setq n1 0 
       in 1
       l_l '()
 ) ;setq end
 (repeat 4
     (setq n1 (+ n1 1)
	   n2 (* in 10)
	   in (1+ in)
	   at (substr str n1 10)
     ) ;setq end      
     (setq n1 n2
	   at (* (atof at) 1000)
	   l_l (cons at l_l)
      ) ;setq end
 ) ;repeat end
 (repeat 5
     (setq n1 (+ n1 1)
	   n2 (* in 10)
	   in (1+ in)
	   at (substr str n1 10)
	   at (atof at)
	   n1 n2
	   l_l (cons at l_l)
      ) ;setq end
 ) ;repeat end
 (setq l_l (reverse l_l))
) ;defun end;
(defun C:SAP( )
 (setvar "cmdecho" 0)
 (command "layer" "m" "m33" "c" "1" "" "s" "0" "")
 (setq fina (getstring "\nfile name:")
       file (open fina "r")
       l-ref 2000.0
       M33-max 1000
       htxt (* 0.2 l-ref)
 ) ;setq end
 (while (setq str (read-line file))
	(setq l_str (tran str)
	      p1 (list (nth 0 l_str) (nth 1 l_str)) 
	      p2 (list (nth 2 l_str) (nth 3 l_str)) 
	      dist (/ (distance p1 p2) 4.0)
	      M33-1 (nth 4 l_Str)
	      M33-2 (nth 5 l_Str)
	      M33-3 (nth 6 l_Str)
	      M33-4 (nth 7 l_Str)
	      M33-5 (nth 8 l_Str)
	      l_M33 (list M33-1 M33-2 M33-3 M33-4 M33-5)
	      ang (angle p1 p2)
	      ang-txt (* (- ang (/ pi 2.0)) (/ 180 pi))
	      ang1 (+ ang (/ pi 2.0))
	      index 0
	) ;setq end
	(command "layer" "s" "0" "")
	(command "line" p1 p2 "")
	(command "layer" "s" "m33" "")
	(Setq M33 (nth index l_M33)
	      txt (rtos M33)
	      m33 (* (/ M33 M33-max) l-ref)
	      pl1 (polar p1 ang1 m33)
	      index (1+ index)
	  ;    htxt (* 4.0 scale)
	      ptxt (polar p1 ang1 (* 10 scale))
	      ptxt (polar ptxt ang (* 5 scale))
	 ) ;setq end
	 (command "line" p1 pl1 "")
	 (command "text" ptxt htxt ang-txt txt)
	 (repeat 4
		(Setq M3 (nth index l_M33)
		      txt (rtos M3)
		      m33 (* (/ M3 M33-max) l-ref)
		      p1 (polar p1 ang dist)
		      pl2 (polar p1 ang1 m33)
		      index (1+ index)
		) ;setq end
		(command "line" pl1 pl2 "")
		(setq pl1 pl2)
	  ) ;repeat end
	(Setq txt (rtos M3)
	      pl2 (polar p2 ang1 m33)
	      ptxt (polar p2 ang1 (* 10 scale))
	      ptxt (polar ptxt ang (* -5 scale))
	 ) ;setq end
	 (command "line" p2 pl2 "")
	 (command "text" ptxt htxt ang-txt txt)
 ) ;repeat end
(setvar "cmdecho" 1)
)defun end
