;|
   命令：txt_align
   
   功能：用于TEXT文字的排版
        可以把选定的多行TEXT文字按照“左、中、右”对齐方式排版
        程序去掉文字的前导和后缀空格，同时修改TEXT实体的对齐方式，
        方便以后的文字编辑保持对齐方式。

        程序配合XDRX_API build 20630＋版本使用，朋友们可以把这个LISP拷贝到“晓东工具箱”的安装的
        LISP目录，自己加入到菜单里面就可以非常方便的使用了。
        关于程序的建议请到“晓东CAD空间-编程申请”论坛
        http://www.xdcad.com/forum留言         
        
|;
(defun c:txt_align (/ ss basept basept1 sel txt box tstyl intl p)
  (prompt "\n请选取要排版的文字<退出>:")
  (if (and
	(setq ss (ssget '((0 . "text"))))
	(setq basept (getpoint "\n请拾取对齐位置点<退出>:"))
      )
    (progn
      (xdrx_begin)
      (xdrx_ucson)
      (initget 128 "L R M")
      (if (not (setq sel
		      (getkword
			"\n对齐方式：[L -左对齐 / R -右对齐 / M -中心对齐]<L>:"
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
		"\n如需调整位置, 请点取要移到的位置 <不调整>: "
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
