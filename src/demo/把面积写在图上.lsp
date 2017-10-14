;; write area value of a close polygon to drawing
(DEFUN C:EA(/ OLDOS PT STA QAREA)
;;
  (setq olderr *error*)
  (setq *error* myerr)
  (SETVAR "CMDECHO" 0)
  (SETQ OLDOS (GETVAR "OSMODE"))
;;
  (SETVAR "OSMODE" 0)
  (SETQ STA (CAR (ENTSEL)))
  (COMMAND "AREA" "E" STA)
  (SETQ QAREA (RTOS (GETVAR "AREA") 2 2))
  (SETQ PT (GETPOINT"\nPick the point you want to write area:"))
  (COMMAND "TEXT" PT "" "" QAREA)
;;  
  (SETVAR "OSMODE" OLDOS)
  (SETVAR "CMDECHO" 1)
  (setq *error* olderr)
  (PRINC)
)