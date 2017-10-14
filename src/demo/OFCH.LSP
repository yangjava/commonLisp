;--------------------------
; TRIM4:    |    |
;        ---+    +-----
;
;        ---+    +-----
;           |    |
;
(defun C:OFCH()
 (setq oer *error* *error* clmerr)
 (setq txt-la (getstring "\nlayer name:"))
(while T 
 (setvar "cmdecho" 1)
  (command "offset" pause pause pause "")
 (setvar "cmdecho" 0)
  (command "change" "l" "" "p" "la" txt-la "")
 (setvar "cmdecho" 1)
) ;while end
) ;defun end
;

;
(defun C:TRIM3() 
 (setq ss (ssget))
 (setvar "cmdecho" 0)
 (setq n (sslength ss))
 (setq index 0)
  (setq l_p1 '()
	l_p2 '()
  ) ;setq end
 (repeat n
  (setq ent (entget (ssname ss index)))
  (setq index (+ 1 index))
  (setq type (assoc 0 ent))
  ;(if (= "LINE" (cdr type))
	(setq p1 (cdr (assoc 10 ent))
	      p2 (cdr (assoc 11 ent))
	      l_p1 (cons p1 l_p1) 
	      l_p2 (cons p2 l_p2)
	) ;setq end
  ;)  ;if end
 ) ;repeat end
(setq index 0
       l_int '()
 )
 (repeat (1- n)
	(setq p1  (nth index l_p1)
	      p2  (nth index l_p2) 
	      index1 (1+ index)
	      index (1+ index)
	) ;setq end
	(repeat (- n index)
	 (setq p3 (nth index1 l_p1) 
	       p4 (nth index1 l_p2)
	       index1 (1+ index1)
	 ) ;setq end
	 (setq pt (inters p1 p2 p3 p4)
	       l_int (if pt (cons pt l_int) l_int)
	 ) ;setq end
	)  ;repeat end
 ) ;repeat end
 (setq index 0)
 (setq p (getpoint "\nside to trim:"))
 (repeat n
  (setq entn (ssname ss index)
	index (1+ index)
  ) ;setq end
  (entdel entn)
 ) ;repeat end
 (setq p3 (nth 0 l_int)
       p4 (nth 1 l_int)
       index 0
 ) ;setq end
 (repeat 3
	(setq p1  (nth index l_p1)
	      p2  (nth index l_p2) 
	      index (1+ index)
	 ) ;setq end
	 (setq ang1 (angtos (angle p1 p2) 0 1)
	       ang2 (angtos (angle p1 p3) 0 1)
	       ang3 (angtos (angle p1 p4) 0 1)
	 ) ;setq end
	 (if (= ang1 ang2 ang3)
	     (progn
		 (setq dist1 (distance p1 p3)
		       dist2 (distance p1 p4)
		 ) ;set end
		 (if (< dist1 dist2)
		  (progn
		   (command "line" p1 p3 "")
		   (command "line" p4 p2 "")
		  ) ;progn end
		  (progn
		   (command "line" p1 p4 "")
		   (command "line" p3 p2 "")
		  ) ;progn end
		) ;if end
	     (setq pi1 p1 pi2 p2)
	     ) ;progn end
	 )  ;if end
 ) ;repeat end
 (Setq index 0)
 (repeat 3
	(setq p1  (nth index l_p1)
	      p2  (nth index l_p2) 
	      index (1+ index)
	 ) ;setq end
	 (if (and (/= p1 pi1) (/= p2 pi2))
	     (progn
		(setq ang1 (angtos (angle p2 p1) 0 1)
		      ang2 (angtos (angle p3 p1) 0 1)
		      ang3 (angtos (angle p4 p1) 0 1)
		      int1 (if (= ang1 ang2) p3 p4)
		      int2 (if (= ang1 ang2) p4 p3)
		) ;setq end
		(Setq ang (angle int2 int1)
		      limax (getvar "limmax")
		      max-x (nth 0 limax)
		      max-y (nth 1 limax)
		      max (sqrt (+ (* max-x max-x) (* max-y max-y)))
		      max1 (* -1 max)
		      p1-ext (polar p ang max)
		      p2-ext (polar p ang max1)
		      ang1 (angle p2 p1)
		      p11 (polar p1 ang1 max)
		      p21 (polar p2 ang1 max1)
		      p-int (inters p11 p21 p1-ext p2-ext)
		      dist1 (distance int1 p1)
		      dist2 (distance p-int p1)
		      p1 (if (> dist1 dist2) int1 p1)
		      p2 (if (> dist1 dist2) p2 int1)
		   ) ;setq end
		(command "line" p1 p2 "")
	     ) ;progn end
	 ) ;if end              
 ) ;repeat end       
 (setvar "cmdecho" 0)
);defun end
