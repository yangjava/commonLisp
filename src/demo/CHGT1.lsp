;�ֲ�����
(defun find2(e os ns / as st s nsl osl sl si chf)
   (setq chf nil 
         si 1
         as (assoc 1 e)
         s  (cdr as)
         osl (strlen os)
         nll (strlen ns)
    ) ;setq end
    (while (= osl (setq sl (strlen
         (setq st (substr s si osl)))))
         (if (= st os)
             (setq s (strcat (substr s 1 (1- si)) ns
                                  (substr s (+ si osl)))
                   chf t ; Found old string
                   si (+ si nsl)
              ) ;setq end
              (setq si (1+ si))
          ) ;if end
     ) ;while 2
     (if chf s nil) ;����ҵ����������ַ��������򷵻�nil
) ;defun end

