;+-------------------------------------+
;+��LINEʵ���Ϊָ����ȵĶ�����       +
;+�ļ��� CHWID2.lsp                    +
;+-------------------------------------+
(defun C:chwid2(/ ss txsize n index ent ty oldsize newsize ent1)
 (inivar) ;��ʼ��ϵͳ����
 (setq ss (ssget)
       wid (getdist "\n�����µĶ����߿��:")
       n (sslength ss)
       index 0
 ) ;setq end
 (repeat n
  (setq entname (ssname ss index) ;ȡʵ����
        ent (entget entname)
        index (+ 1 index)
        ty (assoc 0 ent) ;ȡʵ������
  ) ;setq end
  (if (= "LINE" (cdr ty))
   (progn
    (setq p1 (cdr (assoc 10 ent))
          p2 (cdr (assoc 11 ent))
          la (cdr (assoc 8 ent))  )
    (entdel entname)
    (command "layer" "s" la "")
    (command "pline" p1 "w" wid wid p2 "")
   ) ;progn end
  )  ;if end
 )  ;repeat end
 (resvar) ;��ԭϵͳ����
 (princ)
) ;defun end
