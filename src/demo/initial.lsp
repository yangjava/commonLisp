;ϵͳ������ʼ��
(defun inivar()
   ;���û���趨������Ĭ����Ϊ100
  (if (<= (getvar "userr1") 0) (setvar "userr1" 100))
   ;��ȡϵͳ����
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

;ϵͳ������ԭ
(defun resvar()
   (setvar "CMDECHO"  cmdold)
   (setvar "OSMODE"   osmold)
   (setvar "BLIPMODE" bliold)
   (setq *error* oer)
   (princ)
) ;defun end
