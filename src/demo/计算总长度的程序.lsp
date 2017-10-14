(defun C:cd (/ CURVE TLEN SS N SUMLEN) 
;;��������ͨ��
  (vl-load-com) 
  (setq SUMLEN 0) 
  (setq SS (ssget '((0 . "CIRCLE,ELLIPSE,LINE,*POLYLINE,SPLINE,ARC")))) 
  (setq N 0) 
  (repeat (sslength SS) 
    (setq CURVE (vlax-ename->vla-object (ssname SS N))) 
    (setq TLEN (vlax-curve-getdistatparam 
CURVE 
(vlax-curve-getendparam CURVE) 
       ) 
    ) 
    (setq SUMLEN (+ SUMLEN TLEN)) 
    (setq N (1+ N)) 
  ) 
  (print (strcat "�ܳ���: " (rtos SUMLEN 2 5))) 
  (princ) 
) 