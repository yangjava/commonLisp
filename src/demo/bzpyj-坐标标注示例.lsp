;;;���ļ���bgao.lsp��ȣ������û�����ϵ�±�ʾѡȡ��ľ������ꡣ
;���ɣ�����һ��F8�����ԣ�����ܻ���һ�㾪ϲӴ�� 
;�����ʲôBUG������ϵ�ң�piaoyj@szmedi.com.cn
;��������Ϊzb
(defun c:bz(/ CHO os p0 pxx pyy px py pp entl a b pa ppp paa pbb le len sc alph alf p1 p2 p3 ent pd p11 p12 p21)

 (command "undo" "begin")
 (setq CHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 ;(command "style" "txtp" "txt,hztxt" "" "" "" "" "" "")



(while (equal h1 nil)
  (setq h1 (getreal "\n ����������߶�:"))
;(command "LAYER" "Make" "gaobz" "")
 )   
(setq os (getvar "osmode"))
   
(setvar "osmode" 37)


(setq p0 (getpoint "\n ��ѡ������:"))

(setq pxx (car p0))
(setq pyy (nth 1 p0))
(setq px (rtos pxx 2 3))
(setq py (rtos pyy 2 3))
(setvar "osmode" 0)
(setq pp (getpoint "\n ��ѡ��������:"))	;������pp
(command "line" p0 pp nill)

(command "line" "" pause "")


(setq entl (entget (entlast)))
(setq a (assoc 10 entl))
(setq b (assoc 11 entl))
(setq pa (cdr a))		;������pa
(setq ppp (cdr b))		;������pb
(setq paa (trans pa 0 1))
(setq pbb (trans ppp 0 1))		;��������û�����
(setq le (distance paa pbb))		;��������	
(setq len (* 4.3 h1))
(setq sc (/ len le))
(if (< le len)
  (progn 
   (command "scale" (entlast) "" paa sc "")
   )
)
(setq alph (angle paa pbb))		;��������Ļ���alph 
(setq alf (* 180.0 (/ alph pi)))	;����Ƕ�ֵalf
(setq p1 (polar paa alph (* 0.4 h1)))		
(setq p2 (polar p1 alph (* 4 h1)))
(setq p3 (polar p2 alph 1000))
(setq ent (entlast))
(command "break" ent p2 p3)		;��������߼���

(if (>= alf 105) 
(if (>= alf 255)
 (progn
  (setq pd p1)
  (setq alfa alf)
  (setq p11 (polar pd (+ 1.5708 alph) (* 0.3 h1)))
  (setq p12 (polar pd (+ 4.7124 alph) (* 1.3 h1)));�����ı����
  )
 (progn
  (setq p21 (polar p2 (+ pi alph) h1))	;
  (setq p11 (polar p21 (- alph 1.5708) (* 0.3 h1)))
  (setq p12 (polar p21 (+ alph 1.5708) (* 1.3 h1)));�����ı����
  (setq alfa (+ alf 180))
  )
  )
				;��alf����105ʱ
 (progn
  (setq pd p1)
  (setq alfa alf)
  (setq p11 (polar pd (+ 1.5708 alph) (* 0.3 h1)))
  (setq p12 (polar pd (+ 4.7124 alph) (* 1.3 h1)));�����ı����
  )
)

(command "text" p11 h1 alfa "DADA")
(command "text" p12 h1 alfa "ADAD")

(setvar "osmode" os)			;����ԭ��׽ģʽ
 (command "undo" "end")
 (setvar "CMDECHO" CHO)
 (princ)


)