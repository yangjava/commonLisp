;;From ����� �Ĵ� . �ɶ�
(defun c:myarea()
   (setvar "osmode" 0)
   (setq pt (getpoint "\\nѡȡ�㣺"))
   (while pt 
       (setvar "cecolor" "1")
       (command "bpoly" pt "")
       (setq en (entlast))
       (if (/= en nil)
          (progn
             (command "area" "o" en)
             (setq aa (getvar "area"))
             (redraw en 3)
             (alert (strcat "���:" (rtos aa 2))) 
          );end progn
       );end if
       (entdel en)
       (setvar "cecolor" "bylayer")
       (setq pt (getpoint "\\nѡȡ��:"))
    );end while
    (prin1)
);end fun

