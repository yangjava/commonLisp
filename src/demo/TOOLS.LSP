(defun c:pew1()
 (command "pedit" pause "y" "w" 40 "")
) ;defun end
(defun c:pew2()
 (command "pedit" pause "w" 40 "")
) ;defun end

(defun C:pl1(/ ss txsize n index ent ty oldsize newsize ent1)
 ;把solid实体变成polyline实体 
 (inivar) ;初始化系统变量
 (setq ss (ssget)
       n (sslength ss)
       index 0
       wid (getreal "\n输入多义线宽度: ")
 ) ;setq end
 (repeat n
  (setq name (ssname ss index)
        ent (entget name)
  ) ;setq end
  (setq index (+ 1 index))
  (setq ty (assoc 0 ent))
  (if (= "SOLID" (cdr ty))
   (progn
    (setq p10 (cdr (assoc 10 ent))
          p11 (cdr (assoc 11 ent)) 
          p12 (cdr (assoc 12 ent)) 
          p13 (cdr (assoc 13 ent)) 
    ); setq end
    (entDEL name)
    (command "pline" p10 "w" wid wid  p11 p13 p12 "c")
   )
  )
 )
 (resvar) ;还原系统变量
) 
 
(defun c:bli0() (setvar "blipmode" 0))
(defun c:bli1() (setvar "blipmode" 1))
(defun c:tedit() (command "dim" "tedit" pause pause "e"))
(defun c:os1() (setvar "osmode" 15359))
(defun c:os0() (setvar "osmode" 0))
(defun c:sc1() (command "scale" "W" pause pause "" "int" pause 2))

;尺寸标注刷新
(defun c:dimup() 
   (setvar "cmdecho" 0)
   (princ "\n选择目标：")
   (setq ss (ssget)) 
   (command "dim" "up" ss "" "e")
   (princ)
) ;setq end

(defun ass(d l)
 (setq rei_as (* pi d d 0.25)
       rei_as (* rei_as (/ 1000.0 l))
 ) ;setq end
 (princ "\nas=")
 (princ rei_as)
 (princ " mm2\n")
 (princ)
) ;defun end

;*--打开梁筋层,关闭其他层
(defun c:ol()
 (command "layer" "set" "0" "")
 (command "layer" "off" "*" "y" "")
 (command "layer" "on" "14" "")
 (command "layer" "on" "24" "")
 (command "layer" "on" "11" "")
)

;*--关闭梁筋层,打开其他层
(defun c:cl()
 (command "layer" "set" "0" "")
 (command "layer" "on" "*" "")
 (command "layer" "off" "14" "")
 (command "layer" "off" "24" "")
 (command "layer" "off" "21" "")
 (command "layer" "off" "8" "")
 (command "layer" "off" "11" "")
)

;柱子截面放大
(defun c:sc1()
  (setq oos (getvar "osmode"))
  (setvar "osmode" 32)
  (command "scale" "w" pause pause "" pause 3)
  (setvar "osmode" oos)
) ;end defun

;桩基承载力计算
(defun c:zj()
  (princ "\n输入桩基参数:")
  (setq d1 (getreal "\nd(m):")
        d2 (getreal "\nD(m):")
        xy (getint "\n大桩系数1/3(1) / 1/4(2)")
        index (if (= xy 1) (/ 1.0 3.0) (/ 1.0 4.0))
        pfsai (expt (/ 0.8 d2) index)
        Ap (* pi d2 d2 0.25)
        qpk (getreal "\qpk(kn/m2):")
        R (* pfsai Ap qpk)
        R (/ R 1.65)
  )
  (princ  "\R=")
  (princ R)
) ;defun end

;拉伸指定距离
(defun c:st1() (command "stretch" "c" pause pause "" '(0 0) '(200 0)))
(defun c:st2() (command "stretch" "c" pause pause "" '(0 0) '(-200 0)))
(defun c:st3() (command "stretch" "c" pause pause "" '(0 0) '(0 200)))
(defun c:st4() (command "stretch" "c" pause pause "" '(0 0) '(0 -200)))

(defun c:ro1() 
 (setq ss (ssget)) 
 (command "rotate" ss "" pause 180) 
 (command "move" ss "" pause pause)
)