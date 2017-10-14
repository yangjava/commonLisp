(defun c:scr (/	     big   cmd count  dir	  fname	 name	pt1    pt3
	      pt1-2  pt1-4  pt3-2  pt3-4  pt2	 pt4	pt-list
	      rot    ss	    x1	   y1	  x3	 y3	x0     y0
	     )

  (setq	rot (getvar "viewtwist")
	dir (getvar "dwgprefix")
	cmd (getvar "cmdecho")
  )
  (setvar "cmdecho" 0)

  (while (or (= (setq name (getstring "请输入图名:")) "")
	     (findfile (strcat name ".dwg"))
	 )
  )

  (setq fname (strcat dir name))


  (if (not
	(setq pt1 (getpoint (getvar "extmin") "请选择左下角点："))
      )
    (setq pt1 (getvar "extmin"))
  )
  (if (not (setq pt3 (getcorner pt1 "请选右上择角点：")))
    (setq pt3 (getvar "extmax"))
  )


  (setq	x1 (car pt1)
	y1 (cadr pt1)
  )
  (setq	x3 (car pt3)
	y3 (cadr pt3)
  )
  (if (> x1 x3)
    (setq x0 x1
	  x1 x3
	  x3 x0
    )
  )
  (if (> y1 y3)
    (setq y0 y1
	  y1 y3
	  y3 y0
    )
  )

;;;根据选取的两对角点，考虑扭曲角度，计算另外两角点的坐标
  (setq pt1-2 (polar pt1 (- (* 2.0 pi) rot) 5000))
  (setq pt1-4 (polar pt1 (- (* 0.5 pi) rot) 5000))
  (setq pt3-2 (polar pt3 (- (* 1.5 pi) rot) 5000))
  (setq pt3-4 (polar pt3 (- (* 1.0 pi) rot) 5000))
  (setq pt2 (inters pt1 pt1-2 pt3 pt3-2 nil))
  (setq pt4 (inters pt1 pt1-4 pt3 pt3-4 nil))


  (setq big 0.01)
  (command "pline" pt1 pt2 pt3 pt4 "c")
  (setq ss (entlast))

  (command "zoom" pt1 pt3)
  (command "zoom" "0.95x")
  (setq count 6)

  (repeat 6
    (setq big (* big count))
    (setq pt5 (polar pt1 (- (dms 225) rot) big))
    (setq pt6 (polar pt2 (- (dms 315) rot) big))
    (setq pt7 (polar pt3 (- (dms 45) rot) big))
    (setq pt8 (polar pt4 (- (dms 135) rot) big))
    (setq count (- count 1))
    (command "trim" ss "" "f" pt5 pt6 pt7 pt8 pt5 "" "")
  )
;;;end repeat

  (setq pt-list (list pt1 pt2 pt3 pt4))
  (setq ss (ssget "cp" pt-list))
  (command "wblock" fname "" (list 0 0) ss "")
  (setvar "cmdecho" cmd)
  (princ)

)
;;;
(defun dms (temp / rad)
  (setq rad (/ (* temp pi) 180))
)