(defun C:xy( )
 (inivar) ;初始化系统变量
 (setvar "osmode" 32)
 (setq p00 (getpoint "\n输入标注点:"))
 (command "layer" "S" "0" "")
 (princ "\n输入标注线:")
 (setvar "osmode" 0)
 (command "line" p00 pause pause "")
 (setq p0 (getvar "Lastpoint")
       scale (getvar "userr1")
       p1 (polar p0 pi09 scale)
       p2 (polar p0 pi27 (* 4 scale))
       h_txt (* 3 scale)
       x (nth 0 p00)
       y (nth 1 p00)
       x_txt (strcat "X=" (rtos (/ y 1000.0) 2 3))
       y_txt (strcat "Y=" (rtos (/ x 1000.0) 2 3))
 ) ;setq end
 (command "text" p1 h_txt 0 x_txt)
 (command "text" p2 h_txt 0 y_txt)
 (resvar) ;还原系统变量
 (princ)
) ;defun end
