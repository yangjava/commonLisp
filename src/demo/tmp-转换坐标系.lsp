;ת������ϵ��С����
;��Ҿ�����Ҫ����һ��б�ߴ�ֱ��ֱ�ߣ����С������������á�
;FA ��ѡ��һ��ֱ�ߡ�����Ƕ���ߵĻ�������Ҫ�ڶ��������ѡ�����㡣
;AF���ָ�ֱ������ϵ��
;�ǳ����õĹ��ߡ���������ƺá�
;;�ı�����ϵͳ������ѡ���崹ֱ
(defun c:fa (/ ent1 pt_START pt_END)

  (setq ent1 (entsel "\n��ѡ�����"))

  (cond

    ((= ENT1 NIL) (CHSNAP))

    (
     (= (cdr (assoc 0 (entget (car ent1)))) "LWPOLYLINE")
     (CHSNAP)
    )

    (
     (=
       (cdr (assoc 0 (entget (car ent1))))
       "LINE"
     )
     (progn

       (setq pt_START (cdr (assoc 10 (entget (car ent1)))))

       (setq pt_END (cdr (assoc 11 (entget (car ent1)))))

       (setvar "snapang" (angle pt_START pt_END))
     )
    )
  )
  (setvar "orthomode" 1)
  (princ)

)


(DEFUN
	  CHSNAP
		(/ PT1 PT2)

  (setq pt1 (getpoint "\n��ѡ���һ�㣺"))

  (setq pt2 (getpoint "\n��ѡ��ڶ��㣺"))

  (setvar "snapang" (angle pt1 pt2))

)
(defun c:af() 

(setvar "snapang"0) 


) 