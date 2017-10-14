;;;Change M2 to Ķ

(defun C:m2m(/ oldos e1 e2 el1 el2 dat1 dat2 ans)
 (setq olderr *error*)
 (setq *error* myerr)
 (setvar "cmdecho" 0)
 (setq oldos (getvar "osmode"))
 (setvar "osmode" 0)
 (PRINC "\nPick the number to be change:")
 (setq e1 (ENTSEL))
 (setq el1 (entget (car e1)))
 (if (/=  "TEXT" (cdr (assoc 0 el1)))
   (progn
    (princ "\nYour choises not number, try again!")
    (exit)
   )
  )
 (setq dat1 (cdr (assoc 1 el1)))

 ;;;; change
 (setq ans (* (/ (atof dat1) 10000.0) 15.0))
 (setq ans (rtos ans 2 2))
 (setq ans (strcat ans "Ķ"))
 ;;;; write answer
 (setq pt (getpoint "\nPick the point which answer to write:"))
 (command "text" pt "" "" ans)
 (setvar "osmode" oldos) 
 (setvar "cmdecho" 1)
 (setq *error* olderr)
 (princ)
)

;;;Change Ķ to M2

(defun C:mu2m(/ oldos e1 e2 el1 el2 dat1 dat2 ans)
 (setq olderr *error*)
 (setq *error* myerr)
 (setvar "cmdecho" 0)
 (setq oldos (getvar "osmode"))
 (setvar "osmode" 0)
 (PRINC "\nPick the number to be change:")
 (setq e1 (ENTSEL))
 (setq el1 (entget (car e1)))
 (if (/=  "TEXT" (cdr (assoc 0 el1)))
   (progn
    (princ "\nYour choises not number, try again!")
    (exit)
   )
  )
 (setq dat1 (cdr (assoc 1 el1)))

 ;;;; change
 (setq ans (* (/ (atof dat1) 15.0) 10000.0))
 (setq ans (rtos ans 2 2))
 ;;;; write answer
 (setq pt (getpoint "\nPick the point which answer to write:"))
 (command "text" pt "" "" ans)
 (setvar "osmode" oldos) 
 (setvar "cmdecho" 1)
 (setq *error* olderr)
 (princ)
)