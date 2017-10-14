;;; By ???? 1999-06-23
;;; Modify time 1999-08-30
;;; write area value to drawing

(DEFUN C:QA(/ OLDOS PT STA QAREA)
  (setq olderr *error*)
  (setq *error* myerr)
  (SETVAR "CMDECHO" 0)
  (SETQ OLDOS (GETVAR "OSMODE"))
  (SETVAR "OSMODE" 0)
  (SETQ PT (GETPOINT"\nPick the point:"))
  (SETQ STA (BPOLY PT))
  (IF (= STA NIL) (EXIT))
  (COMMAND "AREA" "E" "L")
  (SETQ QAREA (RTOS (GETVAR "AREA") 2 2))
  (COMMAND "TEXT" PT "" "" QAREA)
  (SETVAR "OSMODE" OLDOS)
  (SETVAR "CMDECHO" 1)
  (setq *error* olderr)
  (PRINC)
)