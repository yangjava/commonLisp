(defun c:BLT1 ()
;(IF (= (TYPE F1) 'FILE ) "YES"  (STOP))
;(IF (= (TYPE F2) 'FILE ) "YES" (STOP))
(COMMAND "SETVAR" "CMDECHO" 0)
(SETQ NY (GETVAR "CLAYER"))
(setq BLSET (findfile "c:/acad/ACAD/SUPPORT/BL.SET"))
(IF (= BLSET NIL)
    (PROGN
       (SETQ TEST 1)
       (PROMPT "\nPLEASE SETTING LAYER SKT-BLT :" )
       (SETQ FC (OPEN "c:/acad/ACAD/SUPPORT/BL.SET" "w"))
       (setq BLCNT (GETSTRING T"\nSETTING SKT-BLT LAYER OF CONTNIUE LINE/<0>:"))
       (IF (= BLCNT "")
              (WRITE-LINE "0" FC)
              (WRITE-LINE BLCNT FC)
       )
       (setq BLCEN (GETSTRING T"\nSETTING SKT-BLT LAYER OF CENTER LINE/<CENTER>:"))
       (IF (= BLCEN "")
              (WRITE-LINE "CENTER" FC)
              (WRITE-LINE BLCEN FC)
       )
       (setq BLHID (GETSTRING T"\nSETTING SKT-BLT LAYER OF HIDDEN LINE/<HIDDEN>:"))
       (IF (= BLHID "")
              (WRITE-LINE "HIDDEN" FC)
              (WRITE-LINE BLHID FC)
       )
       (setq BLDIM (GETSTRING T"\nSETTING SKT-BLT LAYER OF DIMESION /<TEXT>:"))
       (IF (= BLDIM "")
              (WRITE-LINE "TEXT" FC)
              (WRITE-LINE BLDIM FC)
       )
       (setq BLTXT (GETSTRING T"\nSETTING SKT-BLT LAYER OF TEXT /<TEXT>:"))
       (IF (= BLTXT "")
              (WRITE-LINE "TEXT" FC)
              (WRITE-LINE BLTXT FC)
       )
       (CLOSE FC)
    )
)

(IF (/= BLSET NIL)
    (PROGN
       (SETQ FC (OPEN "c:/acad/ACAD/SUPPORT/BL.SET" "r"))
       (SETQ BLCNT (READ-LINE FC))
       (SETQ BLCEN (READ-LINE FC))
       (SETQ BLHID (READ-LINE FC))
       (SETQ BLDIM (READ-LINE FC))
       (SETQ BLTXT (READ-LINE FC))
       (IF (OR (= BLCNT "") (= BLCEN "") (= BLHID "") (= BLDIM "") (= BLTXT "")
               (= BLCNT NIL) (= BLCEN NIL) (= BLHID NIL) (= BLDIM NIL) (= BLTXT NIL))
           (CLOSE FC)
       )
       (IF (OR (= BLCNT "") (= BLCEN "") (= BLHID "") (= BLDIM "") (= BLTXT "")
               (= BLCNT NIL) (= BLCEN NIL) (= BLHID NIL) (= BLDIM NIL) (= BLTXT NIL))
           (PROGN
              (PROMPT "\nPLEASE SETTING LAYER OF SCREW :" )
              (SETQ FC (OPEN "c:/acad/ACAD/SUPPORT/BL.SET" "w"))
              (setq BLCNT (GETSTRING T"\nSETTING SKT-BLT LAYER OF CONTNIUE LINE/<0>:"))
              (IF (= BLCNT "")
                  (WRITE-LINE "0" FC)
                  (WRITE-LINE BLCNT FC)
              )
              (setq BLCEN (GETSTRING T"\nSETTING SKT-BLT LAYER OF CENTER LINE/<CENTER>:"))
              (IF (= BLCEN "")
                  (WRITE-LINE "CENTER" FC)
                  (WRITE-LINE BLCEN FC)
              )
              (setq BLHID (GETSTRING T"\nSETTING SKT-BLT LAYER OF HIDDEN LINE/<HIDDEN>:"))
              (IF (= BLHID "")
                  (WRITE-LINE "HIDDEN" FC)
                  (WRITE-LINE BLHID FC)
              )
              (setq BLDIM (GETSTRING T"\nSETTING SKT-BLT LAYER OF DIMESION/<TEXT>:"))
              (IF (= BLDIM "")
                  (WRITE-LINE "TEXT" FC)
                  (WRITE-LINE BLDIM FC)
              )
              (setq BLTXT (GETSTRING T"\nSETTING SKT-BLT LAYER OF TEXT /<TEXT>:"))
              (IF (= BLTXT "")
                  (WRITE-LINE "TEXT" FC)
                  (WRITE-LINE BLTXT FC)
              )
           (CLOSE FC)
           )
       )
       (SETQ FC (OPEN "c:/acad/ACAD/SUPPORT/BL.SET" "r"))
       (SETQ BLCNT (READ-LINE FC))
       (SETQ BLCEN (READ-LINE FC))
       (SETQ BLHID (READ-LINE FC))
       (SETQ BLDIM (READ-LINE FC))
       (SETQ BLTXT (READ-LINE FC))
       (CLOSE FC)
       (PROMPT "\nSCREW CONTINUE OF LAYER IS:" )(PROMPT BLCNT)(TERPRI)
       (PROMPT "\nSCREW CENTER OF LAYER IS:" ) (PROMPT BLCEN)(TERPRI)
       (PROMPT "\nSCREW HIDDEN OF LAYER IS:" ) (PROMPT BLHID)(TERPRI)
       (PROMPT "\nSCREW DIMENSION OF LAYER IS:" ) (PROMPT BLDIM)(TERPRI)
       (PROMPT "\nSCREW TEXT   OF LAYER IS:" ) (PROMPT BLTXT)(TERPRI)
    )
)

(SETQ ANS (strcase (GETSTRING "\nDO YOU WANT TO CHANGE LAST SETTING <Y/N> N:")))
(IF (= ANS "Y")
    (PROGN
       (PROMPT "\nPLEASE SETTING LAYER:" )
       (SETQ FC (OPEN "c:/acad/ACAD/SUPPORT/BL.SET" "w"))
       (PROMPT "\nSCREW CONTINUE OF LAYER <" )(PROMPT BLCNT)(PROMPT">:")
       (setq BLCNT (GETSTRING T))
       (IF (= BLCNT "")
              (WRITE-LINE "0" FC)
              (WRITE-LINE BLCNT FC)
       )
       (PROMPT "\nSCREW CENTER OF LAYER <" )(PROMPT BLCEN)(PROMPT">:")
       (setq BLCEN (GETSTRING T))
       (IF (= BLCEN "")
              (WRITE-LINE "CENTER" FC)
              (WRITE-LINE BLCEN FC)
       )
       (PROMPT "\nSCREW HIDDEN OF LAYER <" )(PROMPT BLHID)(PROMPT">:")
       (setq BLHID (GETSTRING T))
       (IF (= BLHID "")
              (WRITE-LINE "HIDDEN" FC)
              (WRITE-LINE BLHID FC)
       )
       (PROMPT "\nSCREW DIMESION OF LAYER <" )(PROMPT BLDIM)(PROMPT">:")
       (setq BLDIM (GETSTRING T))
       (IF (= BLDIM "")
              (WRITE-LINE "TEXT" FC)
              (WRITE-LINE BLDIM FC)
       )
       (PROMPT "\nSCREW TEXT OF LAYER <" )(PROMPT BLTXT)(PROMPT">:")
       (setq BLTXT (GETSTRING T))
       (IF (= BLTXT "")
              (WRITE-LINE "TEXT" FC)
              (WRITE-LINE BLTXT FC)
       )
       (CLOSE FC)
       (SETQ FC (OPEN "c:/acad/ACAD/SUPPORT/BL.SET" "r"))
       (SETQ BLCNT (READ-LINE FC))
       (SETQ BLCEN (READ-LINE FC))
       (SETQ BLHID (READ-LINE FC))
       (SETQ BLDIM (READ-LINE FC))
       (SETQ BLTXT (READ-LINE FC))
       (CLOSE FC)
       (PROMPT "\nSCREW CONTINUE OF LAYER NOW IS:" )(PROMPT BLCNT)(TERPRI)
       (PROMPT "\nSCREW CENTER   OF LAYER NOW IS:" ) (PROMPT BLCEN)(TERPRI)
       (PROMPT "\nSCREW HIDDEN   OF LAYER NOW IS:" ) (PROMPT BLHID)(TERPRI)
       (PROMPT "\nSCREW DIM      OF LAYER NOW IS:" ) (PROMPT BLDIM)(TERPRI)
       (PROMPT "\nSCREW TEXT     OF LAYER NOW  IS:" ) (PROMPT BLTXT)(TERPRI)
  )
)


;THE POINT IS FIST COUNTER Y THEN X
(MENUCMD "S=BLT")
(SETQ D (GETREAL"\nSelect dim:"))
(GRAPHSCR) (TERPRI)                                     ;GRAPHICS MODE
;(COMMAND "UJ" "")
;(SETQ D 16)
;(SETQ ANS "Y")
(IF  (= (SETQ ANS1 (GETSTRING "\nDOU YOU WANT TO CUT PART LINE (P2) :<Y>")) "")
     (SETQ ANS1 "Y"))
(SETQ ANS1 (STRCASE ANS1))
(IF (= D 3)
    (PROGN
       (SETQ D2 3.3)
       (SETQ BD1 5)
       (SETQ BD2 6)
       (SETQ H1  3)
       (SETQ H2  4)
       (SETQ SP 0.25)
       (SETQ SPP 0.5)
       (SETQ TTH 2.5)
    )
)
(IF (= D 4)
    (PROGN
       (SETQ D2 4.5)
       (SETQ BD1 6.5)
       (SETQ BD2 7.5)
       (SETQ H1  4)
       (SETQ H2  5)
       (SETQ SP 0.35)
       (SETQ SPP 0.7)
       (SETQ TTH 2.5)
    )
)
(IF (= D 5)
    (PROGN
       (SETQ D2 5.5)
       (SETQ BD1 8)
       (SETQ BD2 9)
       (SETQ H1  5)
       (SETQ H2  6.5)
       (SETQ SP 0.4)
       (SETQ SPP 1)
       (SETQ TTH 2.7)
    )
)
(IF (= D 6)
    (PROGN
       (SETQ D2 6.5)
       (SETQ BD1 10)
       (SETQ BD2 11)
       (SETQ H1  6)
       (SETQ H2  7.5)
       (SETQ SP 0.5)
       (SETQ SPP 1)
       (SETQ TTH 2.7)
    )
)
(IF (= D 8)
    (PROGN
       (SETQ D2 8.8)
       (SETQ BD1 13)
       (SETQ BD2 14)
       (SETQ H1  8)
       (SETQ H2  9.5)
       (SETQ SP 0.63)
       (SETQ SPP 1)
       (SETQ TTH 3)
    )
)
(IF (= D 10)
    (PROGN
       (SETQ D2 11)
       (SETQ BD1 16)
       (SETQ BD2 17.5)
       (SETQ H1  10)
       (SETQ H2  11.5)
       (SETQ SP 0.75)
       (SETQ SPP 1)
       (SETQ TTH 3)
    )
)
(IF (= D 12)
    (PROGN
       (SETQ D2 13)
       (SETQ BD1 18)
       (SETQ BD2 20)
       (SETQ H1  12)
       (SETQ H2  13.5)
       (SETQ SP 0.88)
       (SETQ SPP 1)
       (SETQ TTH 3)
    )
)
(IF (= D 14)
    (PROGN
       (SETQ D2 15.5)
       (SETQ BD1 21)
       (SETQ BD2 23)
       (SETQ H1  14)
       (SETQ H2  15.5)
       (SETQ SP 1.0)
       (SETQ SPP 1)
       (SETQ TTH 3)
    )
)
(IF (= D 16)
    (PROGN
       (SETQ D2 18)
       (SETQ BD1 24)
       (SETQ BD2 26)
       (SETQ H1  16)
       (SETQ H2  17.5)
       (SETQ SP 1.0)
       (SETQ SPP 1)
       (SETQ TTH 3)
    )
)
(IF (= D 20)
    (PROGN
       (SETQ D2 22)
       (SETQ BD1 28)
       (SETQ BD2 30)
       (SETQ H1  20)
       (SETQ H2  21.5)
       (SETQ SP 1.25)
       (SETQ SPP 1)
       (SETQ TTH 3)
    )
)
(IF (= D 22)
    (PROGN
       (SETQ D2 24)
       (SETQ BD1 30)
       (SETQ BD2 32)
       (SETQ H1  22)
       (SETQ H2  23.5)
       (SETQ SP 1.25)
       (SETQ SPP 1)
       (SETQ TTH 3)
    )
)
(IF (= D 24)
    (PROGN
       (SETQ D2 26)
       (SETQ BD1 33)
       (SETQ BD2 35)
       (SETQ H1  24)
       (SETQ H2  25.5)
       (SETQ SP 1.5)
       (SETQ SPP 1)
       (SETQ TTH 3)
    )
)
(IF (= D 30)
    (PROGN
       (SETQ D2 33)
       (SETQ BD1 42)
       (SETQ BD2 45)
       (SETQ H1  30)
       (SETQ H2  32)
       (SETQ SP 1.75)
       (SETQ SPP 1)
       (SETQ TTH 3)
    )
)
(IF (= D 36)
    (PROGN
       (SETQ D2 39)
       (SETQ BD1 50)
       (SETQ BD2 54)
       (SETQ H1  36)
       (SETQ H2  38)
       (SETQ SP 1.75)
       (SETQ SPP 1)
       (SETQ TTH 3)
    )
)
;(SETQ P1 (LIST -5.31 3.64))
;(SETQ P2 (LIST -5.31 133.64))
(SETQ P1 (GETPOINT "\nP1:"))
(SETQ P2 (GETPOINT P1 "\nP2:"))
(COMMAND "OSNAP" "NONE")
(SETQ AG1 (ANGLE P1 P2))
(SETQ AG2 (* PI 0.5))
(SETQ LSE (DISTANCE P1 P2)) ;DIST FOR P1 P2
(SETQ CR SP)
(SETQ DR3 (POLAR (POLAR P1 AG1 H2) (+ AG1 AG2) (/ BD2 2.0)))
(SETQ DR3M (POLAR (POLAR P1 AG1 H2) (+ AG1 AG2) 0))
(SETQ LG (DISTANCE P2 DR3M))
(SETQ LP (GETVAR "LUPREC"))   ;LP DIGITAL
(SETQ EXDG (- LG (FIX LG)))
(SETQ LG1 (FIX LG))
(SETQ LG1 (ITOA LG1))
(SETQ SN (STRLEN LG1))
(SETQ DN (SUBSTR LG1 SN SN))
(SETQ DN1 (ATOI DN))
(IF (< DN1 5)
    (PROGN
       (SETQ DNG 0)
       (SETQ MT DN1) ;MLUS VALUE
    )
)
(IF (>= DN1 5)
    (PROGN
       (SETQ DNG 5)
       (SETQ MT (- DN1 5))
    )
)

(SETQ EXMT (+ MT EXDG)) ;EXTEND MLUS VALUE
(SETQ MIDEXT (- LG EXMT))
(SETQ EXMT1 EXMT)
(SETQ EXMT 0)

(SETQ DR15-1 (POLAR (POLAR P1 AG1 (+ H2 EXMT)) (+ AG1 AG2) (- (/ D 2.0) SP)))
(SETQ DR16-1 (POLAR (POLAR P1 AG1 (+ H2 EXMT)) (- AG1 AG2) (- (/ D 2.0) SP)))
(SETQ DR27 (POLAR (POLAR P1 AG1 (+ LSE (* D 1.5))) (+ AG1 AG2) (- (/ D 2.0) SP)))
(SETQ LONG1 (DISTANCE DR15-1 DR27))
(SETQ LONG1 (RTOS LONG1 2 1))
(SETQ LTEST (STRLEN LONG1))
(SETQ LTEST1 (SUBSTR LONG1 (- LTEST 2) 3))
(SETQ LTEST1 (ATOF LTEST1))
(SETQ LTEST2 0)

(IF (>= LTEST1 2.49)
    (SETQ LTEST2 (- 5 LTEST1))
)
(IF (< LTEST1 2.49)
    (SETQ LTEST2 (* LTEST1 -1))
)

(IF (AND (>= LTEST1  7.5) (<  LTEST1 9.99))
    (SETQ LTEST2 (- 10 LTEST1))
)
(IF (AND (> LTEST1  5.01) (< LTEST1 7.5))
    (SETQ LTEST2 (- LTEST1 10))
)
(SETQ DR27 (POLAR (POLAR P1 AG1 (+ LSE (* D 2.0) LTEST2)) (+ AG1 AG2) (- (/ D 2.0) SP)))
(SETQ DR28 (POLAR (POLAR P1 AG1 (+ LSE (* D 2.0) LTEST2)) (- AG1 AG2) (- (/ D 2.0) SP)))
(SETQ DR29 (POLAR (POLAR P1 AG1 (- (+ LSE (* D 2.0) LTEST2) SP)) (+ AG1 AG2) (/ D 2.0)))
(SETQ DR30 (POLAR (POLAR P1 AG1 (- (+ LSE (* D 2.0) LTEST2) SP)) (- AG1 AG2) (/ D 2.0)))
(SETQ LONG1 (DISTANCE DR15-1 DR27))
(SETQ LONG1 (RTOS LONG1 2 0))
(SETQ DR21 (POLAR (POLAR P1 AG1 (- LSE (* D 0.6))) (+ AG1 AG2) (/ D 2.0)))
(SETQ DR22 (POLAR (POLAR P1 AG1 (- LSE (* D 0.6))) (- AG1 AG2) (/ D 2.0)))
(SETQ DR23 (POLAR (POLAR P1 AG1 (- LSE (* D 0.6))) (+ AG1 AG2) (- (/ D 2.0) SP)))
(SETQ DR24 (POLAR (POLAR P1 AG1 (- LSE (* D 0.6))) (- AG1 AG2) (- (/ D 2.0) SP)))
(SETQ DR25 (POLAR (POLAR P1 AG1 (- LSE (* D 0.6) SP)) (+ AG1 AG2) (/ D 2.0)))
(SETQ DR26 (POLAR (POLAR P1 AG1 (- LSE (* D 0.6) SP)) (- AG1 AG2) (/ D 2.0)))

(SETQ DR1 (POLAR P1 (+ AG1 AG2) (/ BD2 2.0)))
(SETQ DR2 (POLAR P1 (- AG1 AG2) (/ BD2 2.0)))
(SETQ DR3 (POLAR (POLAR P1 AG1 (+ H2 EXMT)) (+ AG1 AG2) (/ BD2 2.0)))
(SETQ DR4 (POLAR (POLAR P1 AG1 (+ H2 EXMT)) (- AG1 AG2) (/ BD2 2.0)))
(SETQ DR5 (POLAR (POLAR P1 AG1 (+ 1 EXMT)) (+ AG1 AG2) (- (/ BD1 2.0) SP)))
(SETQ DR6 (POLAR (POLAR P1 AG1 (+ 1 EXMT)) (- AG1 AG2) (- (/ BD1 2.0) SP)))
(SETQ DR7 (POLAR (POLAR P1 AG1 (+ (- H2 H1) SP EXMT)) (+ AG1 AG2) (/ BD1 2.0)))
(SETQ DR8 (POLAR (POLAR P1 AG1 (+ (- H2 H1) SP EXMT)) (- AG1 AG2) (/ BD1 2.0)))
(SETQ DR9 (POLAR (POLAR P1 AG1 (+ H2 EXMT)) (+ AG1 AG2) (/ BD1 2.0)))
(SETQ DR10 (POLAR (POLAR P1 AG1 (+ H2 EXMT)) (- AG1 AG2) (/ BD1 2.0)))
(SETQ DR11 (POLAR (POLAR P1 AG1 (+ H2 EXMT)) (+ AG1 AG2) (+ (/ D 2.0) SPP)))
(SETQ DR12 (POLAR (POLAR P1 AG1 (+ H2 EXMT)) (- AG1 AG2) (+ (/ D 2.0) SPP)))
(SETQ DR13 (POLAR (POLAR P1 AG1 LSE) (+ AG1 AG2) (+ (/ D 2.0) SPP)))
(SETQ DR14 (POLAR (POLAR P1 AG1 LSE) (- AG1 AG2) (+ (/ D 2.0) SPP)))
(SETQ DR15 (POLAR (POLAR P1 AG1 (+ H2 EXMT)) (+ AG1 AG2) (/ D 2.0)))
(SETQ DR16 (POLAR (POLAR P1 AG1 (+ H2 EXMT)) (- AG1 AG2) (/ D 2.0)))
(SETQ DR17 (POLAR (POLAR P1 AG1 (- (+ LSE (* D 2.5)) SP)) (+ AG1 AG2) (/ D 2.0)))
(SETQ DR18 (POLAR (POLAR P1 AG1 (- (+ LSE (* D 2.5)) SP)) (- AG1 AG2) (/ D 2.0)))
(SETQ DR19 (POLAR (POLAR P1 AG1 (+ LSE (* D 2.5))) (+ AG1 AG2) (- (/ D 2.0) SP)))
(SETQ DR20 (POLAR (POLAR P1 AG1 (+ LSE (* D 2.5))) (- AG1 AG2) (- (/ D 2.0) SP)))


(SETQ SINT (POLAR (POLAR P1 AG1 (+ LSE (* D 2.5))) AG1 (/ D 2.0)))
(SETQ DRINT1 (POLAR SINT (+ AG1 (DTR 120)) 3))
(SETQ DRINT2 (POLAR SINT (- AG1 (DTR 120)) 3))

(SETQ INT1 (INTERS SINT DRINT1 DR19 DR23 NIL))
(SETQ INT2 (INTERS SINT DRINT2 DR20 DR24 NIL))


(IF (= ANS1 "Y")
    (PROGN
       (SETQ P2N (POLAR P2 (+ AG1 AG2) 8))
       (COMMAND "SELECT" P2N "")
       (SETQ SA (SSGET "P" ))
       (SETQ N (SSLENGTH SA))
       (SETQ I 0)
       (SETQ NB (ASSOC 0 NA))
       (SETQ NA (ENTGET (SSNAME SA I)))
       (SETQ NP11 (ASSOC 10 NA))
       (SETQ NP12 (ASSOC 11 NA))
       (SETQ LA1 (ASSOC 8 NA))
       (SETQ LA11 (CDR LA1))
       (SETQ NP1 (CDR NP11))
       (SETQ NP2 (CDR NP12))
       (SETQ TR1 NP1)
       (SETQ TR2 NP2)
       (COMMAND "ERASE" P2N "")
       (IF (> (CAR TR1) (CAR TR2))
          (PROGN
             (SETQ TR1 NP2)
             (SETQ TR2 NP1)
          )
       )
    )
)
(COMMAND "LAYER" "SET" BLHID "")
(IF (= ANS1 "Y")
    (COMMAND "LAYER" "SET" BLCNT "")
)
(COMMAND "LINE" DR1 DR3 DR4 DR2 "")
(COMMAND "LINE" DR5 DR7 "")
(COMMAND "LINE" DR6 DR8 "")
(COMMAND "LINE" DR5 DR6 "")
(COMMAND "LINE" DR7 DR8 "")
(COMMAND "LINE" DR7 DR9 "")
(COMMAND "LINE" DR8 DR10 "")
(COMMAND "LINE" DR11 DR13 "")
(COMMAND "LINE" DR12 DR14 "")
(COMMAND "LINE" DR15 DR29 "")
(COMMAND "LINE" DR16 DR30 "")
(COMMAND "LINE" SINT INT1 "")
(COMMAND "LINE" SINT INT2 "")
(COMMAND "LINE" INT1 INT2 "")
(COMMAND "LINE" INT1 DR27 "")
(COMMAND "LINE" INT2 DR28 "")
(COMMAND "LINE" DR27 DR29 "")
(COMMAND "LINE" DR28 DR30 "")
(COMMAND "LINE" DR27 DR28 "")
(COMMAND "LINE" DR29 DR30 "")

(IF (= ANS1 "Y")
    (COMMAND "LAYER" "SET" BLTXT "")
)
(IF (>= (DISTANCE DR15-1 DR27) (* D 3))
    (PROGN
      (COMMAND "LINE" DR25 DR23 DR27 "")
      (COMMAND "LINE" DR26 DR24 DR28 "")
      (COMMAND "LINE" DR21 DR22 "")
    )
)
(IF (<  (DISTANCE DR15-1 DR27) (* D 3))
    (PROGN
      (COMMAND "LINE" DR15-1 DR27 "")
      (COMMAND "LINE" DR16-1 DR28 "")
    )
)

(COMMAND "LINE" DR17 DR18 "")
(COMMAND "LINE" DR17 DR19 "")
(COMMAND "LINE" DR17 DR19 "")
(COMMAND "LINE" DR17 DR29 "")
(COMMAND "LINE" DR18 DR30 "")
(COMMAND "LINE" DR19 DR20 "")
(COMMAND "LINE" DR18 DR20 "")

(IF (= ANS1 "Y")
    (PROGN
       (COMMAND "LAYER" "SET" LA11 "")
       (COMMAND "LINE" TR1 (POLAR (POLAR P1 AG1 LSE) (+ AG1 AG2) (/ D 2.0)) "")
       (COMMAND "LINE" TR2 (POLAR (POLAR P1 AG1 LSE) (- AG1 AG2) (/ D 2.0)) "")
    )
)


(COMMAND "LAYER" "SET" BLCEN "")
(COMMAND "LINE" (POLAR P1 AG1 -2) (POLAR P1 AG1 (+ 2 LSE (* D 3))) "")

;HERE IS TEXT

(SETQ NUM (GETSTRING T "\nHOW MANY BLT-SKT ?"))
(IF (OR (= NUM "") (= NUM "1"))
   (SETQ BLDIMT " ")
   (SETQ BLDIMT (STRCAT "(" NUM "X" ")"))
)
(IF (<= D 8)
    (SETQ TT2 (GETPOINT"\nWHERE IS TEXT POINT:"))
    (SETQ TT2 (POLAR (POLAR P1 AG1 (+ H1 3 EXMT)) (- AG1 AG2) -1))
)
(IF (<  D 10)
    (SETQ TT5 (POLAR (POLAR P1 AG1 (- LSE 11.5))  (+ AG1 AG2) 2.6))
    (SETQ TT5 (POLAR (POLAR P1 AG1 (- LSE 11.5))  (+ AG1 AG2) 4.0))
)

(SETQ LP (GETVAR "LUPREC"))   ;LP DIGITAL
(SETQ D1 (RTOS D 2 LP))
(SETQ D11 (FIX (ATOF D1)))
(SETQ RET (- D D11))
(IF (OR (= RET 0) (= RET 0.0) (= RET 0.00))
    (PROGN
       (SETQ SR (- (STRLEN D1) LP 1))
       (SETQ D1 (SUBSTR D1 1 SR))
    )
)
(SETQ DIST1 (STRCAT "M" D1 "X" LONG1 "L" BLDIMT))

(COMMAND "LAYER" "SET" BLTXT "")
(COMMAND "TEXT" TT2 TTH (RTD AG1) DIST1)
(COMMAND "LAYER" "SET" NY "")
)
