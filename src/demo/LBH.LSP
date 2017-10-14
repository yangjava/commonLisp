;+------------------------+
;�ļ���: LBH.lsp          +
;+------------------------+
;�б��ַ�����text��������ַ�����txt_f,�������ڵ�λ��,���򷵻�0
(defun iiif(text txt_f)
 (setq  num  1)
  (while (and (<= num (strlen text))
              (not (= (substr text num 1) txt_f)) 
         ) ;and end
      (setq num (+ num 1)) ;setq end   
  ) ;while end
  (if (<= num (strlen text)) num 0)
) ;defun end

(defun C:lbh(/ ss txsize n index ent ty oldsize newsize ent1 ind1)
 (inivar) ;��ʼ��ϵͳ����
 (princ "\nѡ�������:")
 (setq ss (ssget)
       n (sslength ss)
       index 0
       ind1  0
 ) ;setq end
 (command "style" "rmc" "simplex,hztxt" 0 0.7 0 "N" "N") ;��������
 (princ "\n���ڴ��������Ժ�......")
 (repeat n
  (setq ent (entget (ssname ss index)) ;��ȡʵ���ϵ��
        entname (ssname ss index)      
        index (+ 1 index)
        ty (cdr (assoc 0 ent))
  ) ;setq end
  ;�б��Ƿ����ַ�"L"���ַ���
  (if (and (= ty "TEXT") (> (iiif (cdr (assoc 1  ent)) "L") 0))  ;if 1
    (progn  ;progn 1
     (setq la   (cdr (assoc 8 ent))  ;����
           alfa (cdr (assoc 50 ent)) ;�ַ��Ƕ�
           h    (cdr (assoc 40 ent)) ;�ַ��߶�
           p1   (cdr (assoc 10 ent)) ;�ַ����
           txt  (cdr (assoc 1  ent)) ;�ַ��ı�
           txt1 (substr txt 2 1)
           num  (iiif txt " ")  ;���ҿո�
     ) ;setq end
    (if (> num 0) ;if 2
     (progn ;progn 2
      (setq t2 (substr txt 1 (1- num)) ;���ַ������ո�ǰ��ֳ�����
            t1 (substr txt (+ num 1))
            p2 (polar p1 (+ alfa pi09) (+ h (* 0.4 h)))
            alfa (/ (* alfa 180) pi)  ;����ת�Ƕ�
            ind1 (1+ ind1)
      ) ;setq end
      (entdel entname) ;ɾ��ԭ���ַ�
      (command "layer" "s" la "")  ;�л����ַ������ڵĲ�
      (command "text" p1  h alfa t1) ;����д�ַ���
      (command "text" p2  h alfa t2)
     ) ;progn 2
    ) ;if2 end
   ) ;progn1 end
  ) ;if1 end
 ) ;repeat end
 (princ (strcat "\n����������� " (rtos ind1 2 0) "��"))
 (resvar) ;��ԭϵͳ����
 (princ)
) ;defun end