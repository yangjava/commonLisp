;;һ��������򣬳�����Խ�һ�Ŵ�ͼ�ֳ�������Сͼ��
;;Ҫ��
;;1��������e:����һ��plotĿ¼����plotĿ¼Ϊ��
;;2����һ��ѡ��Ŀ�����ѡһ��ͼ�ţ��ַ�����3�����Ժ��ͼ�ų�����Զ�����
(defun c:bb()
  (command "osnap" "off")
 (princ "\nѡ��ͼ��")
 (setq s (ssget))
  (setq th 0)
  (setq xxx 0)
      (setq name (ssname s 0))
      (setq a (entget name))
      (setq t1 (assoc '0 a))
      (setq t1 (cdr t1))
      (if (= t1 "TEXT") (PROGN
          (setq tx (assoc '1 a))
          (setq tx (cdr tx))
          (setq llen (strlen tx))
          (setq llen (- llen 2))
          (setq tx1 (substr tx 1 llen))
          ))
  (while (/= xxx nil)
    (if (< th 10)(setq thh (strcat "0" (rtos th 2 0))))
    (if (>= th 10)(setq thh (rtos th 2 0)))
    (setq name1 (strcat tx1 thh))
    (setq th (+ th 1))
    (setq p1 (getpoint"\n �����һ��:"))
    (setq xxx p1)
    (if (/= p1 nil)(progn
        (setq p2 (getpoint"\n ����ڶ���:"))
        (setq mmm (strcat "e:/" "plot" "/" name1))
        (command "-wblock" mmm "" p1 "w" p1 p2 "")
        (command "oops")
        )
  )
 )
 )

