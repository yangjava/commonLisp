;--------------------------------------+
;�ļ�����M-AS.lsp                      +
�������������������㡡������������ ��+
;--------------------------------------+
(defun c:M-AS( / c lx b h gj fy ksib m mode fc h0 tt w as) 
 (inivar) ;��ʼ��ϵͳ����
 (textscr)
 (setq c (if (setq temp (getreal "\n��ǿ�ȵȼ�:<20>"))  temp 20) 
       lx (if (= (setq temp (getstring "�����(1)/��(2):<1>")) "") "1" temp)
       b (if (= lx "2") (getreal "����(mm)��") 1000)
       h (if (= lx "1") (getreal "���(mm):") 
                        (getreal "����(mm):"))
       gj (if (setq temp (getreal "�ֽ�ȼ�:<2>"))  temp 2)
       fy (if (= gj 1) 210 310)
       ksib (if (= gj 1) 0.614 0.544) ;������ѹ���߶�
       m (getreal "���(KN-m):")
       mode (getint "\n���ɹ淶GB10-89(0)/���¹淶GB50010-2002(1)����:<1>")
       mode (if mode mode 1)
 ) ;setq end
 (if (null (type concrete)) (load "concrete.lsp")) 
 (setq fc (concrete "fc" c mode)) ;��ѯ������������ѹǿ��
 (if (= mode 1) (setq fc (* fc (alfa1 c)))) ;������¹淶
 (setq h0 (if (= lx "1") (- h 20) (- h 35)) ;������Ч��ѹ���߶�
       tt (/ (* m 1e6) (* fc b h0 h0))      
       w (- 1 (sqrt (- 1 (* 2.0 tt))))
       as (/ (* fc b h0 w) fy ) 
 ) ;setq end
 (if (<= w ksib) 
   (progn 
      (princ (strcat "\n�����ѹ���߶�= " (rtos w 2 4)))
      (princ (strcat "\n������= " (rtos as 2 2) "(mm2)" ))
   ) ;progn end
   (progn 
      (princ (strcat "\n�����ѹ���߶�=" (rtos w 2 4) " >������ѹ���߶�=" (rtos ksib 2 4)))
      (princ "\n���治��,��Ӵ����!")
   ) ;progn
 ) ;if end
 (resvar) ;��ԭϵͳ���� 
 (princ)
) ;defun end
