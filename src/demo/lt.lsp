;����ʽ¥��
(defun c:lt()
  (setvar "cmdecho" 0)
  (setq txt (strcat "\n������¥����ʽ 1:����������������������2:����������������ƽ�Σ�" 
                   "\n3:�����ƽ�Σ������������4:���¾���ƽ�Σ�5: ���¾�������"
                   "\n6:������¥��:") )
 (princ txt)
 (setq n_type (getstring)
       txt_lib (strcat "lt-" (substr n_type 1 1))
 )
 (princ "\n����ͼ�����㣺")
 (command "insert" txt_lib  "x" 1 "y" 1 "r" 0 pause)
 (setvar "cmdecho" 1)
);defun end