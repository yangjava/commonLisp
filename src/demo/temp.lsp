(setq M 10 fc 12.5 h0 115)
(defun cal_rei(M fc h0 / r s t1 t2 tt w as)
  (setq r 0.614
        s 0.426
        t1 (/ 1.0 (* fc h0 h0))
        t2 (* M 1000.0 t1)
  ) ;setq end
  (if (> t2 s) 
    (progn
      (princ "\n º”¥ÛΩÿ√Ê!")
      (setq as 9999.9)
    ) ;progn end
    (setq tt (/ (* m 1e3) (* fc h0 h0))
          w (- 1 (sqrt (- 1 (* 2.0 tt))))
          as (/ (* fc 1000 h0 w) 210 ) 
    ) ;setq end
  ) ;if end
  (setq as (if (< as (* 0.0015 1000 h0)) (* 0.0015 1000 h0 ) as))
) ;defun end
