
(defun chgterr (s)
   (if (/= s "Function cancelled")   ; If an error (such as CTRL-C) occurs
      (princ (strcat "\nError: " s)) ; while this command is active...
   )
   (setq p nil)                      ; Free selection set
   (setq *error* olderr)             ; Restore old *error* handler
   (princ)
)

(defun C:CHGT(/ p l n e os as ns st s nsl osl sl si chf chm olderr)
   (setq olderr  *error*             ; Initialize variables
         *error* chgterr
         chm     0)
   (setq p (ssget))                  ; Select objects
   (if p 
    (progn                      ; If any objects selected
     (while (< 0 (setq osl (strlen (setq os (getstring t "²éÕÒ×Ö´®: "))))) ;while1 begain
      (setq nsl (strlen (setq ns (getstring t "Ìæ»»×Ö´®: "))))
      (setq l 0 n (sslength p))
      (while (< l n)                 ; For each selected object... while2 begain
         (if (= "TEXT"               ; Look for TEXT entity type (group 0)
                (cdr (assoc 0 (setq e (entget (ssname p l))))))
            (progn
               (setq chf nil si 1)
               (setq s (cdr (setq as (assoc 1 e))))
               (while (= osl (setq sl (strlen
                             (setq st (substr s si osl)))))
                  (if (= st os)
                      (progn
                        (setq s (strcat (substr s 1 (1- si)) ns
                                        (substr s (+ si osl))))
                        (setq chf t) ; Found old string
                        (setq si (+ si nsl))
                      )
                      (setq si (1+ si))
                  )
               )
               (if chf (progn        ; Substitute new string for old
                  (setq e (subst (cons 1 s) as e))
                  (entmod e)         ; Modify the TEXT entity
                  (setq chm (1+ chm))
               ))
            )
         )
         (setq l (1+ l))
      ) ;while2 end
      (princ (strcat "¹²ÕÒµ½×Ö´®'" os "'" (rtos chm 2 0) "´Î\n"))             ; Print total lines changed
      (setq chm 0) 
    ) ;while1 end
    ) ;progn end
   ) ;if end
   (terpri)
   (setq *error* olderr)             ; Restore old *error* handler
   (princ "²éÕÒÍê³É......")
   (princ)
) ;defun end
