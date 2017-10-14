(defun fun1(N a / at pia piat X1 X2 X3 X4 Y1 Y2 Area f)
  (setq at (att a)
	pia (* pi a)
	piat (* pi at)
	X1 (sin (* 2 pia))
	X1 (/ X1 2 pia)
	X0 (- 1 X1)
	Area (* pi R R)
	X1 (* a fcm Area X0)
	txt (strcat "a*fcm*Area*X0=" (rtos a 2 3) "X" (rtos fcm 2 1) "X" (rtos Area 2 2) "X" 
                    (rtos X0 2 3) " =" (rtos X1 2 3))
  ) ;setq end
  (princ txt)
  (setq
	Y1 (- N X1)
 	Y1 (/ Y1 (- a at))
	X2 (sin pia)
	X4 (* X2 X2 X2)
	X2 (* (/ 2.0 3.0 pi) fcm Area R X4)
   ) ;setq end
   (princ "\n2/3fcm*Area*...=")
   (princ X2)
  (setq
	X2 (- (* N yei) X2)
	X3 (sin piat)
	X3 (+ X2 X3)
	Y3 (* Rs X3 (/ 1.0 pi))
	Y2 (/ X2 Y3)
 ) ;setq end
 (princ "\Y3=")
 (princ Y3)
 (princ "\nY1=")
 (princ Y1)
 (princ "\nY2=")
 (princ Y2)
 (princ)
 (princ "\nf=")
 
 (setq 	f (- Y1 Y2))

) ;defun end

(fun1 N 0.5)

       