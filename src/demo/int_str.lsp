
;+-------------------------------------------+
;�������ַ��Զ�ȡ������50��00��β            +
;������Զ�ɸѡ��TEXT��MTEXTʵ��              +
;�ļ�����INT_str.lsp 			            +
;---------------------------------------------+
(defun C:INT_str(/ ss txsize n index ent type oldsize newsize ent1)
  (inivar) ;��ʼ��ϵͳ����
  (princ "\nѡ������: ")
  (setq ss (ssget)
        n (sslength ss)
        sel1 1
        index 0
  ) ;setq end
 (repeat n   ;��ѡ�е�ʵ����һ����
  (setq ent (entget (ssname ss index)))   ;��entget����ȡʵ���ϵ��
  (setq index (+ 1 index))
  (setq type (assoc 0 ent))    ;��assoc����ȡʵ�������ӱ�
  (if (OR (= "TEXT" (cdr type)) (= "MTEXT" (CDR TYPE)) ) ;������TEXT,MTEXT
   (progn
    (setq oldsize (assoc sel1 ent)   ;ȡ�����ӱ�
          old_txt (cdr oldsize)
          old_num (atoi old_txt)
          new_num (* 50 (+ 1 (/ old_num 50)))  ;������ȡ����50
          new_txt (itoa new_num)
          newsize (cons (car oldsize) new_txt) ;�������ӱ�
          ent1 (subst newsize oldsize ent)    ;�����ӱ��滻���ӱ�
          oldsize (assoc 72 ent)
          newsize (cons (car oldsize) '0)
          ent2 (subst newsize oldsize ent1)
    ) ;setq end
    (if (> old_num 300)
      (entmod ent2)  ;��ͼ�����ݿ����޸�ʵ���ϵ����Ϣ
    ) ;if end
   )  ;prong end
  )  ;if end
 ) ;repeat end
(resvar) ;��ԭϵͳ����
) ;defun end
