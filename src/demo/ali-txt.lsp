;+-------------------------------------------+
;�ַ�����                                    +
;�ļ�����ALI-TXT.lsp       		     +
;--------------------------------------------+
;�����ַ����
(defun mylen(ss / index nn1 nn2 txt )
   (setq index 1 
         nn1 (strlen ss)
         nn2 0)
   (while (<= index nn1)
        (setq txt (substr ss index 1))
        (cond 
          ((= txt "1")  ;���Ϊ�ַ�"1",ֻ�����ַ���
             (setq index (1+ index) 
                   nn2 (+ nn2 0.5) 
             ) ;setq end
           ) ;condition 1
          ((> (ascii txt) 134)  ;���Ϊ����,�����ַ���һ���ַ���
             (setq index (+ 2 index) 
                   nn2 (+ nn2 1) 
             ) ;setq end
           ) ;condition 2
          (T (setq index (1+ index) nn2 (1+ nn2)))
         ) ;cond end
     ) ;while end
     (setq nn2 nn2) ;����nn2
) ;defun end

;(defun C:ali-txt(/ ss txsize n index ent ty oldsize newsize ent1)
(defun C:ali-txt()
  (inivar) ;��ʼ��ϵͳ����
  (princ "\nѡ������: ")
  (setq ss (ssget)
        p0 (getpoint "\n��ָ�������:")
        mode (getint "\n���뷽ʽ:�����(1)/�Ҷ���(2)/�϶���(3)/�¶���(4)/�ж���(5):")
        n (sslength ss)
        index 0
  ) ;setq end
  (repeat n   ;��ѡ�е�ʵ����һ����
   (setq ent (entget (ssname ss index)))   ;��entget����ȡʵ���ϵ��
   (setq index (+ 1 index))
   (setq ty (assoc 0 ent))    ;��assoc����ȡʵ�������ӱ�
   (if (OR (= "TEXT" (cdr ty)) (= "MTEXT" (cdr ty)) ) ;������TEXT,MTEXT
    (progn
     (setq old_sub (assoc 10 ent)   ;ȡ���ָ߾��ӱ�
           p_old (cdr old_sub)
           h_txt (cdr (assoc 40 ent))      ;ȡ�ָ�
           w     (cdr (assoc 41 ent)) ;ȡ�ָ߿��
           txt_txt (cdr (assoc 1 ent)) ;ȡ�ַ���
           len_txt (mylen txt_txt)    ;�����ַ�����
     ) ;setq end
     (cond
       ((= mode 1) ;�����
             (setq ali_p (list (nth 0 p0) (nth 1 p_old)))
       ) ;;�����
       ((= mode 2) ;�Ҷ���
             (setq l_txt (* w len_txt h_txt)  ;�����ַ��ܿ��
                   p1 (polar p0 pi18 l_txt) 
                   ali_p (list (nth 0 p1) (nth 1 p_old))
             ) ;setq end
       ) ;�Ҷ���
       ((= mode 3) ;�϶���
           (setq p1 (polar p0 pi27 h_txt)
                 ali_p (list (nth 0 p_old) (nth 1 p1))
           ) ;setq end
       ) ;�϶���
       ((= mode 4) ;�¶���
             (setq ali_p (list (nth 0 p_old) (nth 1 p0)))
       ) ;�¶���
       ((= mode 5) ;�ж���
             (setq l_txt (* 0.5 w len_txt h_txt)  ;�����ַ��ܿ�ȵ�һ��
                   p1 (polar p0 pi18 l_txt) 
                   ali_p (list (nth 0 p1) (nth 1 p_old))
             ) ;setq end
       ) ;�ж���
     ) ;cond end
     (setq new_sub (cons (car old_sub) ali_p) ;�������ӱ�
           ent1 (subst new_sub old_sub ent)    ;�����ӱ��滻���ӱ�
     ) ;setq end
     (entmod ent1)  ;��ͼ�����ݿ����޸�ʵ���ϵ����Ϣ
    )  ;prong end
   )  ;if end
  ) ;repeat end
 (resvar) ;��ԭϵͳ����
 (princ)
) ;defun end
