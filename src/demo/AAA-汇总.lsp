;;��Ϊ�ҵ�lisp�ܻ�
;���γ���ɽ�ѡ�е�Բһ���Բü���Բ�У�����Բ�ĵģ����߶Ρ�
(Defun c:tc (/ ucsfl fil len n m e ed pt0 pt pbx) 
 (command "redraw")
 (command "undo" "begin")
 (setq pbx (getvar "pickbox"))
 (setvar "pickbox" 3)
 (setq ucsfl (getvar "ucsfollow"))
 (setvar "ucsfollow" 0)
 (command "ucs" "world")			;ת����������ϵ
 (setq fil (ssget))			��ѡ��Բ
 (setq len (sslength fil))		��ȷ��ѡ��Բ�ĸ���
 (setq n 0)
 (while (<= n (- len 1))
   (setq e (ssname fil n))
   (setq ed (entget e))
   (if (/= "CIRCLE" (cdr (assoc 0 ed)))
    (setq n (+ 1 n))
    (progn 
     (setq pt0 (cdr (assoc 10 ed)))	;old circle's center
(command "zoom" "c" pt0 "80")		;�ɵ�������������Ӧ��ͬ�뾶��Բ
      (setq pt (osnap pt0 "nea"))
       (setq m 1)
      (while (and (/= nil pt) (< m 10)) ;��һ��Բ�ڲü�������10�Σ��Է�ֹ��ѭ��
          (command "trim" e "" pt "")
          (setq pt (osnap pt0 "nea"))
          (setq m (+ 1 m))
      )
      (setq n (+ 1 n))
(command "zoom" "p")	
    )
   )
  )
 (command "ucs" "p")			;�����û�����ϵ
 (setvar "ucsfollow" ucsfl)
 (setvar "pickbox" pbx)
 (command "undo" "end")
(princ)      
)
(princ "\n\n\t\ttcpiao\t\t\t�ü�Բ��ͨ��Բ�ĵ�ֱ��\t\t  tc")
;*********************************************************************************
;���γ���ɽ�ѡ�е�Բһ����תΪ������Ҫ�İ뾶��
(Defun c:cr (/ ucsfl R fil len n e ed b c R0 pt0 ang pt1 pt2 ln lnn i j jt qt k L pt ) 
 (command "redraw")
 (command "undo" "begin")
 (setq pbx (getvar "pickbox"))
 (setvar "pickbox" 3)
 (setq ucsfl (getvar "ucsfollow"))
 (setvar "ucsfollow" 0)
 (command "ucs" "world")			;ת����������ϵ
 ( setq R (getreal "\ninput R "))	������Բ�뾶
 (setq fil (ssget))			��ѡ��Բ
 (setq len (sslength fil))		��ȷ��ѡ��Բ�ĸ���
 (setq n 0)
 (while (<= n (- len 1))
  (progn 
   (setq e (ssname fil n))
   (setq ed (entget e))
   (setq b nil) (setq c nil)
   (if (/= "CIRCLE" (cdr (assoc 0 ed)))
    (setq n (+ 1 n))
    (progn 
     (setq R0 (cdr (assoc 40 ed)))	;old circle's radius
     (setq pt0 (cdr (assoc 10 ed)))	;old circle's center
     (setq ed (subst (cons 40 R) (assoc 40 ed) ed))
     (setq n (+ 1 n))
     (entmod ed)
(command "zoom" "c" pt0 "15")		;�ɵ�������������Ӧ��ͬ�뾶��Բ
     (setq ang 0)
     (while (< ang (* 2 pi))
      (setq pt1 (polar pt0 ang R0))
      (setq ang (+ ang (/ pi 30)))
      (setq pt2 (osnap pt1 "endp"))
      (setq b (cons pt2 b))
     )
(setq ln (length b))
(setq lnn (- ln 1))
(setq i 0)
(setq pt (nth i b))
(while (<= i lnn)
  (setq pt (nth i b))
  (setq j (+ i 1))
  (setq jt (nth j b))
  (while (and (not (equal pt jt)) (< j lnn))
    (setq qt (nth j b))
    (setq j (+ 1 j))
    (setq jt (nth j b))
  )
  (if (not (equal pt jt)) (setq c (cons pt c)))
  (setq i (+ 1 i))
)
  (setq k 0)
  (setq L (length c))
;*************************************
  (while (<= k (- L 1))
    (setq pt (nth k c))
       (if (> R0 R)
          (command "extend" e "" pt "")
          (command "trim" e "" pt "")
       )
    (setq k (+ 1 k))
   )��while����
;*************************************
(command "zoom" "p")
  )
 )
    )
   )
 (command "ucs" "p")			;�����û�����ϵ
 (setvar "ucsfollow" ucsfl)
 (setvar "pickbox" pbx)
 (command "undo" "end")
(princ)      
)
(princ "\n\n\t\t crpiao\t\t�ı��龮�뾶�ĳ���\t\t\t cr")
;*********************************************************************************
;;;���ļ���bgao.lsp��ȣ������û�����ϵ�±�ʾѡȡ��ľ������ꡣ
;���ɣ�����һ��F8�����ԣ�����ܻ���һ�㾪ϲӴ�� 
;�����ʲôBUG������ϵ�ң�piaoyj@szmedi.com.cn
;��������Ϊzb
(defun c:zb( / ucsfl os p0 pxx pyy px py pp ppp paa pbb entl a b le sc len alph alf alfa p1 p2 p3 p11 p22 p21 t1 t2)
 (command "undo" "begin")
 (setq CHO (getvar "CMDECHO"))		;CMDECHOϵͳ����Ϊ1ʱ����ʹ��command����ʱ������ʾ�����룬��Ϊ0�򲻷���
 (setvar "CMDECHO" 0)
 ;(command "style" "standard" "txt,hztxt" "" "" "" "" "" "")
(setq ucsfl (getvar "ucsfollow"))
(setvar "ucsfollow" 0)
(command "ucs" "world")			;ת����������ϵ
(setq os (getvar "osmode"))

(while (equal h nil)
  (setq h (getreal "\n ����������߶�:"))
;(command "LAYER" "Make" "gaobz" "")
 )   
(setvar "osmode" 37)			;1�˵㣬4Բ�ģ�32���㣬����ϡ�
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
(setq p2 (polar p1 alph (* 7 h)))	;p2�����߳�
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
 (command "undo" "end")
 (setvar "CMDECHO" CHO)
 (princ)
)
(princ "\n\n\t\t zbpyj\t\t\t��ע����ĳ���\t\t\tzb")
;*********************************************************************************
(defun c:pg()
(command "purge" "a" "" "n")
(command "purge" "a" "" "n")
(command "purge" "a" "" "n")
(command "purge" "a" "" "n")
)
(princ "\n\n\t\t auto-purge\t\tpurge������������\t\t\tpg")
;*********************************************************************************
(Defun c:et ( / newtx v1 v2 nme oldtx v3 )
(setvar "cmdecho" 0)
       (prompt "\npick text to be changed:")
       (setq v1 (ssget))
       (setq newtx (getstring T "\nENTER NEW STRING: " ))
       (setq newtx (cons 1 newtx))
       (setq v2 0)
          (if (/= v1 nil)
              (while (< v2 (sslength v1))
                     (setq nme (ssname v1 v2 ))
                     (setq oldtx (assoc 1 (entget nme)))
                     (setq v3 (entget nme))
                     (entmod (subst newtx oldtx v3))
                     (entupd nme)
                     (setq v2 (+ v2 1 ))
               )
         )
 )
(princ "\n\n\t\t et\t\t\t��ָ�����������ѡ���ı�\t\t  et")
;*********************************************************************************
;���ɣ�����һ��F8�����ԣ�����ܻ���һ�㾪ϲӴ�� 
;�����ʲôBUG������ϵ�ң�piaoyj@szmedi.com.cn
;��������Ϊbz
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
   
(setvar "osmode" 4)				;��׽��ʽԭ37
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
(setq p2 (polar p1 alph (* 3.5 h1)))
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
  (setq p21 (polar p2 (+ pi alph) (* 0.5 h1)))	  ;(* 0.5 h1)���������עʱ���ı����
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
(princ "\n\n\t\tbzpyj\t\t\t������ע��龮��ߵĹ���\t\t  bz")
;*********************************************************************************
;author piao yingjun    piaoyj@szmedi.com.cn
(Defun c:cb ( / fil sc scl len n e ed h) 
 (command "redraw")
(setq fil (ssget))
( setq sc (getreal "\n�������ȱ�����"))
( setq scl sc)
(setq len (sslength fil))
(setq n 0)
 (while (<= n (- len 1))
  (progn
   (setq e (ssname fil n))
   (if (= "TEXT" (cdr (assoc 0 (setq ed (entget e)))))   
     (progn
       (setq h  scl)
          (setq ed (subst (cons 41 h) (assoc 41 ed) ed)) 
          (setq n (+ 1 n))
          (entmod ed)
     )
    (setq n (+ 1 n))
    )
   )
  )
(princ)
)
(princ)
(princ "\n\n\t\t�������ȱ���cb\t  �ı���ѡ�ı��Ŀ�ȱ���\t\t   cb")
;*********************************************************************************
;;;  peditn.lsp
;;;  (C)  ����ˮ�� 
;;;  by zhuxiaofeng
;;;  1995,10,20 (1��)
;;;  1997,5,15  (2��)
;;;  2001,8,15  (3��) by piaoyingjun
;;;  �ó�����޸���,��,Բ����Բ�������ߵĿ��
(defun C:pn (/ p l n e q w a m layer0 color0 linetype0 layer1 color1 linetype1 rad-out rad-in)
 (command "undo" "begin")
  (setq oldblp (getvar "blipmode")
        oldech (getvar "cmdecho")
        olderr *error*
        linetype1 (getvar "celtype")
        layer1 (getvar "clayer")
        color1 (getvar "cecolor")
  )
  (setvar "blipmode" 0) 
  (setvar "cmdecho" 0)
  (defun *error* (msg)
    (princ "\n") 
    (princ msg)
    (setvar "blipmode" oldblp)
    (setvar "cmdecho" oldech)
    (setq *error* olderr)
    (princ)
  )  
  (prompt "\n��ѡ��Ҫ�ı��ȵ���,��,Բ��������.")
  (setq p (ssget))
  (setq w (getreal "\n��������<1>:"))
  (if (not w) (setq w 1))
  (setq l 0 m 0 n (sslength p))
  (while (< l n)
    (setq q (ssname p l))
    (setq ent (entget q))
    (setq b (cdr (assoc 0 ent)))
    (if (member b '("LINE" "ARC"))
      (progn 
        (command "PEDIT" q "y" "w" w "x") 
        (setq m (+ 1 m))
      ) 
    )
    (if (= "LWPOLYLINE" b)
      (progn 
        (command "PEDIT" q "w" w "x") 
        (setq m (+ 1 m))
      ) 
    )
    (if (= "CIRCLE" b)
      (progn 
        (if (assoc 6 ent) (setq linetype0 (cdr (assoc 6 ent))) (setq linetype0 "bylayer"))
        (setq layer0 (cdr (assoc 8 ent)))
        (if (assoc 62 ent) (setq color0 (cdr (assoc 62 ent))) (setq color0 "bylayer"))
        (setq center0 (cdr (assoc 10 ent)))
        (setq radius0 (cdr (assoc 40 ent)))
        (setq diameter0 (* 2 radius0))
        (entdel q)
        (command "color" color0)
        (command "layer" "s" layer0 "")
        (command "linetype" "s" linetype0 "")
        (if (> w diameter0)
          (progn 
            (princ "\n\t ���߿����Բ��ֱ�����ʽ���Բ���")
            (princ)
            (setq rad-out (* 2 radius0)
                  rad-in 0
            )
          )
        )
        (if (<= w diameter0)
          (progn 
            (setq rad-out (+ (* 2 radius0) w) 
                  rad-in (- (* 2 radius0) w)
            )
          )
        )
        (command "donut" rad-in rad-out center0 "")
        (setq m (+ 1 m))
      )
    ) 
    (setq l (+ 1 l))
  )
  (if (= "ELLIPSE" b)
   (progn
    (setq center0 (cdr (assoc 10 ent)))
    (command "offset" w q center0 "")
    (command "offset" w q (list 0 0 0) "")
    (entdel q)
    (command "hatch" "s" "f" center0 (list 0 0 0) "" "")
  ))
  (if (= 0 m)
    (progn 
     (princ "\n\t  û���κ���,��,Բ����Բ���������߱�ѡ��")
      (princ)
    )
  )
  (setvar "blipmode" oldblp)
  (setvar "cmdecho" oldech)
  (setq *error* olderr)
  (command "color" color1)
  (command "layer" "s" layer1 "")
  (command "linetype" "s" linetype1 "")
 (command "undo" "end")
  (princ)

(princ "\n\t  �߿�༭����,  (c) 1997 ")
(princ "\n\t  c:Peditn �Ѽ���; ��Pn��������.\n")
(princ)
)
(princ "\n\n\t\t�߿�༭����\t\t  �ɸı��ߡ�Բ����Բ�����������ߵĿ��\t    pn")
;*********************************************************************************
;author piao yingjun    piaoyj@szmedi.com.cn
(Defun c:cz ( / fil len n e ed b new1 c new2 ) 
 (command "redraw")
(setq fil (ssget))
(setq len (sslength fil))
(setq n 0)
 (while (<= n (- len 1))
  (progn
   (setq e (ssname fil n))
   (setq ed (entget e))
   (if (/= nil)   
     (progn
          (setq b (assoc 10 ed))
         (setq new1 (list 10 (car (cdr b)) (car (cdr (cdr b))) 0))
          (setq ed (subst new1 b ed)) 

          (setq c (assoc 11 ed))
         (setq new2 (list 11 (car (cdr c)) (car (cdr (cdr c))) 0))
          (setq ed (subst new2 c ed)) 
          (setq n (+ 1 n))
          (entmod ed)
     )
    (setq n (+ 1 n))
    )
   )
  )
(princ)
)
;(princ "\n\t  �ı�������,  (piaoyj@szmedi.com.cn) 2001 ")
;(princ "\n\t  ���� �Ѽ���; ��cz��������.\n")
(princ)

(princ "\n\n\t\t��ֱ��������cz\t  �ı��ʵ�壨�Ƕ����ߣ���������Ϊ0   cz")
;*********************************************************************************
(defun c:xs(/ e ed )
(setq e (car (entsel)))
(textscr)
(setq ed (entget e))
)
;(princ "\n\t ��ʾ��ѡʵ�����Ϣ")
;(princ "\n\t �����Ѽ��أ���xs�������\n")
(princ)
(princ "\n\n\t\t��ʾʵ����Ϣxs\t  ��ʾ��ѡ��ʵ�����Ϣ\t\t   xs")
;*********************************************************************************
;author piao yingjun    piaoyj@szmedi.com.cn
(Defun c:cdx ( / case) 
 (command "redraw")

(setq fil (ssget))
(setq case (getint "\n �ĳ�(1)Сд / <ֱ�ӻس�Ϊ��д>��"))	

(setq len (sslength fil))
(setq n 0)

 (while (<= n (- len 1))
  (progn
   (setq e (ssname fil n))
   (cond 
        ((= "TEXT" (cdr (assoc 0 (setq ed (entget e)))))
          (progn
  
           (setq ostr (cdr (assoc 1 ed)))
           (setq nstr (strcase ostr case))
           (setq ed (subst (cons 1 nstr) (assoc 1 ed) ed)) 
           (setq n (+ 1 n))
           (entmod ed)
          ))
        ((= "MTEXT" (cdr (assoc 0 (setq ed (entget e)))))
          (progn
  
           (setq ostr (cdr (assoc 1 ed)))
           (setq nstr (strcase ostr case))
             (setq lenstr (strlen nstr))
              (setq m 1)
              (while (< m lenstr)
                (if (= "\\p" (substr nstr m 2))
                   
                     (setq nstr (strcat (substr nstr 1 m) "\P" (substr nstr (+ 2 m))))
                 ;(setq m (+ 1 m))   
                )
                 (setq m (+ 1 m))
              )

           (setq ed (subst (cons 1 nstr) (assoc 1 ed) ed)) 
           (setq n (+ 1 n))
           (entmod ed)
          ))
       (T (setq n (+ 1 n)))
    )
   )
  )
(princ)
)
(princ "\n\t  �ı���ѡ�ı��Ĵ�Сд ��cdx�������� 2002 \n")
;*********************************************************************************

