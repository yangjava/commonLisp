(defun c:COL( )
  (setvar "cmdecho" 0)
 (setq B (getreal "\ncolumn width(mm):")
       H (getreal "\ncolumn heighth(mm):")
       p0 (getpoint "\ninsert point:")
       scale (getvar "userr1")
       p1 (polar p0 pi27 (/ H 2.0))
       p1 (polar p1 pi18 (/ B 2.0))
       p2 (polar p1 0   B)
       p3 (polar p2 pi09 H)
       p4 (polar p3 pi18 B)
       d1 (polar p1 pi18 (* 10. scale))
       d2 (polar d1 pi09 (/ H 2.0))
       d3 (polar d2 pi09 (/ H 2.0))
       dp1 (polar d1 pi18 (* 10 scale))
       d4 (polar p1 pi27 (* 10 scale))
       d5 (polar d4 0 (/ B 2.0))
       d6 (polar d5 0 (/ B 2.))
       dp2 (polar d4 pi27 (* 10 scale))
 ) ;setq end
 (command "line" p1 p2 p3 p4 "c")
 (command "layer" "s" "pdim" "")
 (setvar "dimdli" 0)
 (command "dim" "ver" d1 d2 dp1 "" "con" d3 "" "e")
 (command "dim" "hor" d4 d5 dp2 "" "con" d6 "" "e")
 (command "layer" "s" "ct" "")
 (setvar "cmdecho" 1)
) ;defun end





