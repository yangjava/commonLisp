;;;���ļ���bgao.lsp��ȣ������û�����ϵ�±�ʾѡȡ��ľ������ꡣ
;���ɣ�����һ��F8�����ԣ�����ܻ���һ�㾪ϲӴ�� 
;�����ʲôBUG������ϵ�ң�piaoyj@szmedi.com.cn
;��������Ϊzb
(defun c:zb( / p0 pxx pyy px py pp ppp paa pbb alph alf alfa p1 p2 p3 p11 p22 p21)
(command"style" "" "" 0 "" "" "" "" "")
(setq ucsfl (getvar "ucsfollow"))
(setvar "ucsfollow" 0)
(command "ucs" "world")			;ת����������ϵ
(setq os (getvar "osmode"))

(while (equal h nil)
  (setq h (getreal "\n ����������߶�:"))
;(command "LAYER" "Make" "gaobz" "")
 )   

   
(setvar "osmode" 37)
(initget 1 "h")
(setq p0 (getpoint "\n H/<ѡ������:>"))
(while (eq p0 "h") 
  (setq hh (getreal "\n ����������߶�:"))
   (if (/= nil hh) (setq h hh))

(setq p0 (getpoint "\n ��ѡ������:"))
)
(setq pxx (car p0))
(setq pyy (nth 1 p0))
(setq px (rtos pxx 2 3))
(setq py (rtos pyy 2 3))
(setvar "osmode" 0)
(setq pp (getpoint "\n ��ѡ��������:"))	;������pp
(command "line" p0 pp nill)
(command "ucs" "p")			;�����û�����ϵ
(command "line" "" pause "")


(setq entl (entget (entlast)))
(setq a (assoc 10 entl))
(setq b (assoc 11 entl))
(setq pa (cdr a))		;������pa
(setq ppp (cdr b))		;������pb
(setq paa (trans pa 0 1))
(setq pbb (trans ppp 0 1))		;��������û�����
(setq le (distance paa pbb))		;��������	
(setq len (* 9.5 h))
(setq sc (/ len le))
(if (< le len)
  (progn 
   (command "scale" (entlast) "" paa sc "")
   )
)
(setq alph (angle paa pbb))		;��������Ļ���alph 
(setq alf (* 180.0 (/ alph pi)))	;����Ƕ�ֵalf
(setq p1 (polar paa alph h))		
(setq p2 (polar p1 alph (* 8.5 h)))
(setq p3 (polar p2 alph 1000))
(setq ent (entlast))
(command "break" ent p2 p3)		;��������߼���

(if (>= alf 105) 
(if (>= alf 255)
 (progn
  (setq pd p1)
  (setq alfa alf)
  (setq p11 (polar pd (+ 1.5708 alph) (* 0.4 h)))
  (setq p12 (polar pd (+ 4.7124 alph) (* 1.4 h)));�����ı����
  )
 (progn
  (setq p21 (polar p2 (+ pi alph) h))	;
  (setq p11 (polar p21 (- alph 1.5708) (* 0.4 h)))
  (setq p12 (polar p21 (+ alph 1.5708) (* 1.4 h)));�����ı����
  (setq alfa (+ alf 180))
  )
  )
				;��alf����105ʱ
 (progn
  (setq pd p1)
  (setq alfa alf)
  (setq p11 (polar pd (+ 1.5708 alph) (* 0.4 h)))
  (setq p12 (polar pd (+ 4.7124 alph) (* 1.4 h)));�����ı����
  )
)
(setq t1 (strcat "X-" py))
(setq t2 (strcat "Y-" px))
(command "text" p11 h alfa t1)
(command "text" p12 h alfa t2)
(setvar "osmode" os)			;����ԭ��׽ģʽ
(setvar "ucsfollow" ucsfl)




)