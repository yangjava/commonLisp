(defun C:DASH( )
 (Setvar "cmdecho" 0)
 (setq scale (getvar "userr1"))
 (command "layer" "s" "rei" "")
 (while (setq p (getpoint "\n输交点:"))
        (setq p1 (polar p (* 0.25 pi) (* 1 scale))
              p2 (polar p (* 1.25 pi) (* 1 scale))
        ) ;setq end
        (command "line" p1 p2 "")
 ) ;while end
 (command "layer" "s" "0" "")
) ;defun end
(defun C:DASH1( )
 (Setvar "cmdecho" 0)
 (command "layer" "s" "rei" "")
 (while (setq p (getpoint "\n输交点:"))
        (setq p1 (polar p (* 0.75 pi) (* 1 scale))
              p2 (polar p (* 1.75 pi) (* 1 scale))
        ) ;setq end
        (command "line" p1 p2 "")
 ) ;while end
 (command "layer" "s" "0" "")
) ;defun end
