
;;;  DDGP.LSP - the good old Garden Path with a new twist.
; Convert angle in degrees to radians
(defun dtr (a)
 (* pi (/ a 180.0))
)
; Acquire information for garden path
(defun gpuser ()
	(setq sp (getpoint "\nStart point of path: "))
	(setq ep (getpoint "\nEndpoint of path: ")) 
	(setq hwidth (getdist "\nHalf width of path: " sp))
;; (setq trad (getdist "\nRadius of tiles: " sp))        ;<-REMOVE  
;; (setq tspac (getdist "\nSpacing between tiles: " sp)  ;<-REMOVE  

(setq pangle (angle sp ep))
	(setq plength (distance sp ep))
	(setq width (* 2 hwidth))
	(setq angp90 (+ pangle (dtr 90))) 
	(setq angm90 (- pangle (dtr 90))) 
)
; Draw outline of path
(defun drawout ()
	(command "pline"
				(setq p (polar sp angm90 hwidth))
				(setq p (polar p pangle plength))
				(setq p (polar p angp90 width))
				(polar p (+ pangle (dtr 180)) plength)
				"close"
	)

)

;Add the following lines of code after the drawout function.

; Call dialog box to set tile specifications 
(defun gp_dialog () 
	(setq tshape "Circle" 
			trad 0.5 
			tspac 0.1 
			tsides 8 )         
	(setq dcl_id  (load_dialog "ddgp.dcl"))  
	;(if (not (new_dialog "gp_box1" dcl_id))(exit))  
	(set_tile "gp_trad" "0.5")  
	(set_tile "gp_spac" "0.1")  
	(mode_tile "gp_side" 1)  
	(set_tile "gp_side" "8")  
	(action_tile "gp_circ" "(setq tshape \"Circle\")(mode_tile \"gp_side\" 1)") 
	(action_tile "gp_poly" "(setq tshape \"Polygon\")(mode_tile \"gp_side\" 0)") 
	(action_tile "cancel" "(done_dialog)(setq gperr \"\")(exit)") 
	(action_tile "accept" 
		(strcat 
			"(progn (setq trad (atof  (get_tile \"gp_trad\")))" 
			"(setq tspac (atof (get_tile \"gp_spac\")))" 
			"(setq tsides (atoi (get_tile \"gp_side\")))" 
                        " (done_dialog))" 
		) 
	) 
	(start_dialog) 
	(unload_dialog dcl_id) 
	(if (= tshape "Circle") 
		(defun gp_tile () (command "circle" p1tile trad)) 
		(defun gp_tile () (command "polygon" tsides p1tile "" trad)) 
	)  
)  
; Define error handler  
(defun gp_err (msg)  
	(setq *error* olderr)  
	(if (not gperr)  
		(princ (strcat "\nGarden path error: " msg))  

(princ gperr)  
	)  
	(if sblip (setvar "blipmode" sblip))  
	(if scmde (setvar "cmdecho" scmde))  
	(princ)  

) 

;The following code should already exist in your ddgp.lsp file, revise as indicated.

; Place one row of tiles the given distance along path
; and possibly offset it
(defun drow(pd offset)
	(setvar "snapang" pangle)                              ;<-NEW 
	(setq pfirst (polar sp pangle pd))
	(setq pctile (polar pfirst angp90 offset))
	(setq p1tile pctile)
	(while (< (distance pfirst p1tile) (- hwidth trad))
		(gp_tile)                                           ;<-NEW 
;;		(command "circle" p1tile trad)                      ;<-REMOVE 

(setq p1tile (polar p1tile angp90 (+ tspac trad trad)))
	)
	(setq p1tile (polar pctile angm90 (+ tspac trad trad)))
	(while (< (distance pfirst p1tile) (- hwidth trad))
		(gp_tile)                                           ;<-NEW 
;;		(command "circle" p1tile trad)                      ;<-REMOVE 
		(setq p1tile (polar p1tile angm90 (+ tspac trad trad))) 
	)
)
; Draw the rows of tiles 
(defun drawtiles ()
	(setq pdist (+ trad tspac))

(setq off 0.0)
	(while (<= pdist (- plength trad))
		(drow pdist off)
		(setq pdist (+ pdist (* (+ tspac trad trad) (sin (dtr 60)))))
		(if (= off 0.0)
			(setq off (* (+ tspac trad trad) (cos (dtr 60))))
			(setq off 0.0)
		)
	)
)
; Execute command, calling constituent functions
(defun C:DDPATH ()                   ; <- REVISE, add "DD" to PATH 
	;(setq olderr *error*                               ; <- NEW 
		;	*error* gp_err                                 ; <- NEW 

sblip nil                                      ; <- NEW 
;			scmde nil                                      ; <- NEW 
;			gperr nil                                      ; <- NEW 
;	)                                                  ; <- NEW 
	(gpuser)
	(setq sblip (getvar "blipmode"))
	(setq scmde (getvar "cmdecho"))
	(setq sang (getvar "snapang"))                     ; <- NEW 
	(setvar "blipmode" 0)
	;(setvar "cmdecho" 0)

(drawout)
	(gp_dialog)                                        ; <- NEW 
	(drawtiles)
	(setvar "blipmode" sblip)
	(setvar "cmdecho" scmde)
	(setvar "snapang" sang)                            ; <- NEW 
	(setq *error* olderr)                              ; <- NEW 
	(princ)
)
; Print message once loaded.                         ; <- NEW 
 
(princ "\nDDGP.LSP Loaded. Type DDPATH to use.")     ; <- NEW 

(princ)      
                                      ; <- NEW 