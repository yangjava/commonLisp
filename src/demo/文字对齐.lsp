;|
   ���txt_align
   
   ���ܣ�����TEXT���ֵ��Ű�
        ���԰�ѡ���Ķ���TEXT���ְ��ա����С��ҡ����뷽ʽ�Ű�
        ����ȥ�����ֵ�ǰ���ͺ�׺�ո�ͬʱ�޸�TEXTʵ��Ķ��뷽ʽ��
        �����Ժ�����ֱ༭���ֶ��뷽ʽ��

        �������XDRX_API build 20630���汾ʹ�ã������ǿ��԰����LISP�����������������䡱�İ�װ��
        LISPĿ¼���Լ����뵽�˵�����Ϳ��Էǳ������ʹ���ˡ�
        ���ڳ���Ľ����뵽������CAD�ռ�-������롱��̳
        http://www.xdcad.com/forum����         
        
|;
(defun c:txt_align (/ ss basept basept1 sel txt box tstyl intl p)
  (prompt "\n��ѡȡҪ�Ű������<�˳�>:")
  (if (and
	(setq ss (ssget '((0 . "text"))))
	(setq basept (getpoint "\n��ʰȡ����λ�õ�<�˳�>:"))
      )
    (progn
      (xdrx_begin)
      (xdrx_ucson)
      (initget 128 "L R M")
      (if (not (setq sel
		      (getkword
			"\n���뷽ʽ��[L -����� / R -�Ҷ��� / M -���Ķ���]<L>:"
		      )
	       )
	  )
	(setq sel "L")
      )
      (xdrx_setsstodb ss 0)
      (while (setq e (xdrx_getentdata 0))
	(setq txt (xdrx_getentdxf 1)
	      box (xdrx_entity_box e)
	)
	(setq basept1 (polar basept (/ pi 2) 100)
	      intl    (inters
			basept
			basept1
			(car box)
			(cadr box)
			nil
		      )
	)
	(setq txt (xdrx_string_trimLeft txt)
	      txt (xdrx_string_trimRight txt)
	)
	(cond
	  ((= "L" sel)
	   (setq basept intl)
	   (xdrx_modent 1 txt 72 0 11 '(0 0 0) 10 intl)
	  )
	  ((= "R" sel)
	   (xdrx_modent 1 txt 11 intl 72 2)
	  )
	  ((= "M" sel)
	   (xdrx_modent 1 txt 11 intl 72 4)
	  )
	)
      )
      ;|(initget 128)
      (if (setq
	    p (xdrx_dragssmove
		"\n�������λ��, ���ȡҪ�Ƶ���λ�� <������>: "
		ss
		basept
		3
		T
	      )
	  )
	(command ".move" ss "" basept p)
      )|;
      (xdrx_ucsoff)
      (xdrx_end)
    )
  )
  (princ)
)
