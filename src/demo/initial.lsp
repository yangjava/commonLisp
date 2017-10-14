;系统变量初始化
(defun inivar()
   ;如果没有设定比例，默认其为100
  (if (<= (getvar "userr1") 0) (setvar "userr1" 100))
   ;读取系统变量
   (setq oer *error* *error* clmerr)
   (setq cmdold (getvar "CMDECHO")
         osmold (getvar "OSMODE")
         bliold (getvar "BLIPMODE")
   ) ;setq end
   (setvar "CMDECHO" 0)
   (setvar "OSMODE" 0)
   (setvar "BLIPMODE" 0)
   (princ)
) ;defun end

;系统变量还原
(defun resvar()
   (setvar "CMDECHO"  cmdold)
   (setvar "OSMODE"   osmold)
   (setvar "BLIPMODE" bliold)
   (setq *error* oer)
   (princ)
) ;defun end
