;
;+-------------------------------------------------------------------------+
;| SETUP.LSP                                                               |
;| Ver.  1.0  Feb. 25, 1986                                                |
;|                                                                         |
;| J.J.K.     Feb. 25, 1986                                                |
;|                                                                         |
;| This program will set the Units and Limits of a new drawing, and will   |
;| draw a border line around the drawing.                                  |
;+-------------------------------------------------------------------------+
;
;
(apply '(lambda ()
 (setvar "cmdecho" 0)
 (menucmd "S=UNITS")
 (setq d nil)
 (setq a (getint "\nSelect the Units from the screen menu: "))
 (menucmd (strcat "S=U" (itoa a)))
 (cond
   (
     (= a 5)
     (setq a 2 d 5)
   )
 )
 (setvar "lunits" a)
 (setq b (getreal "\nSelect the Scale from the screen menu: "))
 (cond
   (
     (= b 0)
     (setq b (getreal "\nEnter the scale: "))
     (setq b (float b))
   )
 )
 (cond
   (
     (= d 5)
     (menucmd "S=METRIC")
   )
   (T
     (menucmd "S=ENGLISH")
   )
 )
 (setq cx (getdist "\nSelect the Paper size from the screen menu: "))
 (setq cy (getdist))
 (cond
   (
     (= cx 0)
     (setq cx (getdist "\nEnter the Horizontal Dimension of the paper: "))
     (setq cy (getdist "\nEnter the Vertical Dimension of the paper: "))
   )
 )
 (setq xl (* b cx) yl (* b cy))
 (command
  "text" '(0 0) (* (getvar "textsize") b) "" "a"
  "erase" "l" ""
  "ltscale" b
  "dim" "dimscale" b "exit"
  "limits" "0,0" (list xl yl)
  "insert" "border" "0,0" xl yl "0"
  "zoom" "a"
 )
 (menucmd "S=S")
)
 '()                                               ; Finishing lambda
)
     ���      LIN     DWG           蝬��  PO      BAK           ܞ���  ARRAYR  LSP           go��G   TABLET  DWG           �y���$ AS      DWG           uo���Y  