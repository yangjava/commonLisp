;;���ڱ�ע��ŵĳ���
;; ϣ���ܹ��Դ�ҵĻ�ͼ������������
;;���ڱ�ע��ŵ�LISP����
;;��ſ����Զ��ۼƱ�ע
;;������ͼ��������6ͼ�����ڱ�ע
;;ϵͳ��Ӧ����"gbeitc","gbcbig"���壬���û�������Ը���
(defun C:DYCY();;;(/ r p1 p2 p3 p4 x y a x1 y1 n nn)
  (defun wz_sz() ;;;��������
  (if (or (=(setq sty (tblsearch "STYLE" "standard")) nil);"����HZ�����Ƿ����"
      (= (cdr (assoc 40 sty)) 0);�ָ��Ƿ�Ϊ��ֵ
      (/= (getvar "TEXTSTYLE") "standard");��ǰ�����Ƿ�Ϊ"hz"
  )
  (command "style" "standard" "gbeitc,gbcbig" (* mm 10) 1 0 "n" "n");��������"
  )
)
(defun tc_sz()
  (if (or (= (setq lay (tblsearch "layer" "6")) nil) ;;;6ͼ���Ƿ����
          (/= (cdr (assoc 2 lay)) "6");��ǰͼ���Ƿ�Ϊ6ͼ��
      )
         (progn    ;;;�������������½�ͼ��
;;LCA - COMMENT: The LAYER command has new options.
            (command "layer" "new" "6" "c" "white" "" "ltype" "continuous" "" "");����"dimͼ��"
            (command "linetype" "s" "continuous" "") ;;;;��������Ϊ��������
            (command "layer" "s" "6" "c" "white" "" "")
          )
         (progn ;;;�ж���������������ͼ��
            (command "layer" "s" "6" "c" "white" "" "")
            (command "linetype" "s" "continuous" "")
          )
  )
)
;;;;������ʼ  
  (setq olderr  *error*    ;;;�����ʼ����
     *error*  yyerr
     oldcmd (getvar "cmdecho")
     oldos (getvar "osmode")
  )
  (setvar "cmdecho" 0)    ;�������
  (setvar "osmode" 0);;;ȡ��׽
  (setq nn 1)
  (if (= mm nil)
    (progn
       (setq mm (getreal "\n �����ͼ����1:mm<1:1>"))
         (if (= mm nil)(setq mm 1))
    )
  )
  (wz_sz)
  (tc_sz)
  ;;;;(setq mmm (* 10 mm))
  (setvar "ORTHOMODE" 0)
  (setq p1 1)
  (while p1
        (setq p1 (getpoint "ѡ����ע�������� <ֹͣ��ע>:"))
 (if p1 ;;*
    (progn
;;;  (setq p1 (getpoint "ָ�����ŵ㣺"))
  (setq p2 (getpoint p1 "ָ����ע�㣺"))
  (if (= no nil)
  (setq no (getint "��ʼ���<1>"))
  )
  (if (= no nil) (setq no 1))
  (setvar "ORTHOMODE" 1)
  (setq p4 (getpoint p2 "ָ����ע����㣺"))
  (setq xx (- (car p4) (car p2)))
  (setq yy (- (cadr p4) (cadr p2))) 
  (setq r (* 8 mm))
  (setq n (getint "\n�������N��?<1>"))
  (if (= n nil) (setq n 1))
  ;(setq n (- n 1));;********
  (setq x (-(car p2)(car p1)))
  (setq y (-(cadr p2)(cadr p1)))
  (setq a (atan (/(abs y)(abs x))))
  (setq y1 (* (sin a) r))
  (setq x1 (* (cos a) r))
        (cond ((and (>= x 0)(>= y 0))
	       (setq p3 (list (+(car p2) x1)(+ (cadr p2) y1)))
	       )
	      ((and (< x 0)(> y 0))
	       (setq p3 (list (-(car p2) x1)(+ (cadr p2) y1)))
	       )
	      ((and (< x 0)(< y 0))
	       (setq p3 (list (-(car p2) x1)(- (cadr p2) y1)))
	       )
	      ((and (> x 0)(< y 0))
	       (setq p3 (list (+(car p2) x1)(- (cadr p2) y1)))
	       )
	 )
  (command "color" "7")
  (command "donut" 0 0.8 p1 "")
  (command "line" p1 p2 "")
  (if (or (<= n 1) (and (> x 0)(< y 0)) (and (> x 0)(= yy 0)) (and (< x 0)(< y 0)(= xx 0)))
    (progn
         (command "circle" p3 (* 8 mm) "")
;;;  (if (and (< x 0)(> n 0))
;;;  (setq no1 (+ no n 1))
;;;  (setq p3 (polar p3 pi (* n 2 r)))
         (command "text" "j" "m" p3 "0" no);**********
         (setq no (+ 1 no))
      )
    )
;;;(if (<= n 0) (command "text" "j" "m" p3 "0" no));**********
;;;  (setq no (+ 1 no))
  (setq x (-(car p4)(car p2)))
  (setq y (-(cadr p4)(cadr p2)))
    (if (and (= x 0) (> y 0))
		  (progn
		    (setq x1 0)
		    (setq y1 (* 2 r))
		   )
    )
    (if (and (= x 0) (< y 0))
		  (progn
		    (setq x1 0)
		    (setq y1 (* 2 (- 0 r)))
		   )
    )
   (if (and (= y 0) (> x 0))
		  (progn
		    (setq y1 0)
		    (setq x1 (* 2 r))
		   )
		 
    )
   (if (and (= y 0) (< x 0))
		  (progn
		    (setq y1 0)
		    (setq x1 (* 2 (- 0 r)))
		   )
		 
    )


  
    (if (and (< x 0)(> n 0))
	  (setq p3 (polar p3 pi (* 2 n r)))
    )
    (if (and (> y 0)(> n 0))
	  (setq p3 (polar p3 (* pi 0.5) (* 2 n r)))
    )
 (if (or (<= n 1) (> x 0) (and (> x 0)(= yy 0)) (and (< x 0)(< y 0)(= xx 0))) (setq n (- n 1)))
 (if (or (and (< y 0) (= xx 0)) (and (< y 0) (= xx 0))) (setq n (- n 1)));;88888888888
    (setq nn0 1)	     
	(while (<= nn0 n)
     (cond ((and (< x 0)(> n 1))
	      (progn
	          (setq p3 (list (- (car p3) x1)(+ (cadr p3) y1)))
	  (command "circle" p3 (* 8 mm) "")
	  (command "text" "j" "m" p3 "0" no )
          (setq no (+ 1 no))
	  (setq nn0 (+ nn0 1))
		)
	    )
            ((and (> y 0) (> n 1))
	        (progn
        	  (setq p3 (list (+ (car p3) x1)(- (cadr p3) y1)))
	  (command "circle" p3 (* 8 mm) "")
	  (command "text" "j" "m" p3 "0" no )
          (setq no (+ 1 no))
	  (setq nn0 (+ nn0 1))
		  )
	     )
	    ((OR (> x 0) (< y 0))
	         (progn
	          (setq p3 (list (+ (car p3) x1)(+ (cadr p3) y1)))
	  (command "circle" p3 (* 8 mm) "")
	  (command "text" "j" "m" p3 "0" no )
          (setq no (+ 1 no))
	  (setq nn0 (+ nn0 1))
         	)
	     )
	  )
   )
      (setvar "ORTHOMODE" 0)
    )
   )
    )
    (command "redraw")
(setq *error* olderr)   ;;;�ָ�ԭ����
    (SETVAR "CMDECHO" OLDCMD)
    (setvar "osmode" oldos)
    (princ)
)