;���ͳ��
;��������������ĳ��򣬹�������ã�������ȫ���ţ�llisp��д
;Ҫ�� 
;1�������Ƶ�ͼ�α����ա�
;2����ȡ����������������û������ʵ�塣
;3����û�����ñ����������ڿ�ʼ���Ҫ���û����������������Ϊ�ա�
(DEFUN C:dlmj (   )
(if (= bl nil)(setq bl(getreal "input bl---:")))(setvar "userr1" bl)
(command "-units" "2" "8" "1" "8" "" "")
(setq pt (getpoint "\n��ȡ�������:"))
(setq mmm 0.0)
  (while (/= pt nil)
  (command "layer" "s" "mj" "")
  (command "-boundary" pt "" )
  (setq ent (entlast)) 
  (command "area" "o" ent) 
  (setq j41 (getvar "area"))
  (command "erase" "l" "")
;  (setq j41 (atof j41))
  (setq j41 (* j41 bl) j41 (* j41 bl))
  (setq j41 (/ j41 1000.0) j41 (/ j41 1000.0))
  (setq mmm (+ mmm j41))
  (setq pt (getpoint "\n�����:"))
  )
(setq nnn (rtos mmm 2 2))
(command "layer" "s" "0" "")
(setq pt (getpoint "\n�����:"))
(command "text" pt "3" "0" nnn "")
(command "-units" "2" "0" "1" "8" "" "")
)

