**���η���***
(defun c:fx()
��������(setq ss(ssget) mun 0)
(repeat (sslength ss)
��������(setq entl-y (ssname ss mun))
��������(command "pedit" entl-y "d" "")
��������(setq entl(entget entl-y))
������ (if (= "POLYLINE" (cdr(assoc 0 entl))) (command "pedit" entl-y "lt" "on" "x"))
������ (if (= 10 (car (nth 13 entl)))��(setq nn 13))
������ (if (= 10 (car (nth 12 entl)))��(setq nn 12)) 
������ (setq lt-name(cdr(assoc 6 entl)) la-name (cdr (assoc 8 entl)))����
��������(setq W_list '())
��������(setq elem (cdr (nth nn entl))) 
��������(setq w_list (append w_list (list elem)))��
������(while (not (null elem))������������������
����(setq nn(+ nn 4))
����(setq elem (cdr (nth nn entl)))�� 
������ (if (= (car (nth nn entl)) 10)
��������(setq w_list (append w_list (list elem)))��;����ʵ��������� 
������ )
������)
��������(setq w_list (reverse w_list))
��������(setq pt1 (car W_list))
��������(command "pline" pt1)
������(while w_list
��������(setq w_list(cdr w_list))
��������(setq pt (car w_list))
��������(command pt)
������)
��������(command "")
��������(command "chprop" (entlast) "" "lt" lt-name "la" la-name "")
��������(command "pedit" (entlast) "lt" "on" "s" "")
��������(command "erase" entl-y "")
��������(setq mun (+ mun 1))
)
)