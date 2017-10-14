(defun c:awa()
  (setvar "cmdecho" 0)
  (initget "05B 06B 08A 08B 10A 12A 16A 20A 24A 28A 32A 40A 48A")
  (setq no (getkword "请输入链号05B,06B,08A,08B,10A,12A,16A,20A,24A,28A,32A,40A或48A<08A>:"))
  (if (= no nil) (setq no "08A"))
  (setq z (getint "请输入齿数<12>:"))
  (if (null z) (setq z 12))
  (setq m (getint "请输入排数<1>:"))
  (if (null m) (setq m 1))
  (setq no (strcase no))
  (cond ((= no "05B") (setq p 8.00) (setq dr 5.00) (setq pt 5.64) (setq b1 3.00))
        ((= no "06B") (setq p 9.525) (setq dr 6.35) (setq pt 10.24) (setq b1 5.72))
        ((= no "08A") (setq p 12.7) (setq dr 7.95) (setq pt 14.38) (setq b1 7.85))
        ((= no "08B") (setq p 12.7) (setq dr 8.51) (setq pt 13.92) (setq b1 7.75))
        ((= no "10A") (setq p 15.875) (setq dr 10.16) (setq pt 18.11) (setq b1 9.4))
        ((= no "12A") (setq p 19.05) (setq dr 11.91) (setq pt 22.78) (setq b1 12.57))
        ((= no "16A") (setq p 25.4) (setq dr 15.88) (setq pt 29.29) (setq b1 15.75))
        ((= no "20A") (setq p 31.75) (setq dr 19.05) (setq pt 35.76) (setq b1 18.9))
        ((= no "24A") (setq p 38.1) (setq dr 22.23) (setq pt 45.44) (setq b1 25.22))
        ((= no "28A") (setq p 44.45) (setq dr 25.4) (setq pt 48.87) (setq b1 25.22))
        ((= no "32A") (setq p 50.8) (setq dr 28.585) (setq pt 58.55) (setbbq b1 31.55))
        ((= no "40A") (setq p 63.5) (setq dr 39.68) (setq pt 71.55) (setq b1 37.85))
        ((= no "48A") (setq p 76.2) (setq dr 47.63) (setq pt 87.83) (setq b1 47.35))
        (t                         (setq dr 7.95) (setq pt 14.38) (setq b1 7.85))
  )
  (cond ((= m 1) (setq bf1 (* 0.93 b1)))
        ((= m 2) (setq bf1 (* 0.91 b1)))
        ((= m 3) (setq bf1 (* 0.91 b1)))
        ((>= m 4) (setq bf1 (* 0.88 b1)))
        (t          (setq bf1 (* 0.93 b1)))
  )
  (setq ba (* p 0.125))
  (setq h (* p 0.5))
  (setq ra (* p 0.04))
  (setq bfm (+ bf1 (* pt (- m 1))))
  (setq ang1 (/ pi z))  ;;;ang1为180/z的弧度值
  (setq d (/ p (sin ang1)))
  (setq da (fix (- (+ d (* p 1.25)) dr)))
  (setq df (- d dr))
  (setq dg (fix (- (* p (/ (cos ang1) (sin ang1))) (* h 1.04) 0.76 dr)))
  (setq pt1 (getpoint "请输入起点:"))
  (setq pt2 (getpoint pt1 "请输入链轮宽度:"))
  (setq l (distance pt1 pt2))
  (setq ang (angle pt1 pt2))
  (setq os (getvar "osmode"))
  (setvar "osmode" 0)
  (setq pt3 (polar pt1 (+ (* pi 0.5) ang) (/ df 2)))
  (setq pt4 (polar pt3 (+ (* pi 0.5) ang) (- (/ (- da df) 2) h)))
  (setq x1 (polar pt1 (+ ang (* 0.5 pi)) (/ da 2)))
  (setq x2 (polar x1 ang bf1))
  (setq xb (polar pt1 ang bf1))
  (setq m1 (polar pt1 (+ ang pi) 2))
  (setq m2 (polar xb ang 2))
  (setq pt5 (polar x1 ang ba))
  (setq pt6 (polar pt5 ang (- bf1 (* ba 2))))
  (setq pt7 (polar pt4 ang bf1))
  (setq pt8 (polar pt3 ang bf1))
  (setq pt9 (polar xb (+ ang (* pi 0.5)) (/ dg 2)))
  (setq pt10 (polar pt2 (+ ang (* 0.5 pi)) (/ dg 2)))
  (setq pt11 (polar pt2 (+ ang (* 1.5 pi)) (/ dg 2)))
  (setq pt12 (polar xb (+ ang (* 1.5 pi)) (/ dg 2)))
  (setq o1 (polar pt7 (+ ang pi) p))
  (setq o2 (polar pt4 ang p))
  (if (= m 1)
    (progn
      (command "pline" pt3 "w" 0 "" pt4 "a" pt5 "l" pt6 "")
      (setq en1 (entlast))
      (command "pline" pt9 "w" 0 "" pt8 pt7 "a" pt6 "")
      (setq en2 (entlast))
      (command "pline" pt9 "w" 0 "" pt10 pt11 pt12 "")
      (command "chamfer" "d" 1 "")
      (command "chamfer" "p" (entlast))
      (command "chamfer" "d" 0 "")
      (command "line" pt3 pt8 "")
      (setq en3 (entlast))
      (command "line" pt3 (polar pt3 (+ ang (* pi 1.5)) df) "")
      (command "mirror" en1 en2 en3 "" pt1 pt2 "n" "")
      (command "mline" "st" "standard" "s" d "j" "z" m1 m2 "")
      (command "change" (entlast) "" "p" "la" "0" "")
      )
  )
  (setvar "osmode" os)
  (princ)
)

(defun c:awb()
  (setvar "cmdecho" 0)
  (initget "05B 06B 08A 08B 10A 12A 16A 20A 24A 28A 32A 40A 48A")
  (setq no (getkword "请输入链号05B,06B,08A,08B,10A,12A,16A,20A,24A,28A,32A,40A或48A<08A>:"))
  (if (= no nil) (setq no "08A"))
  (setq z (getint "请输入齿数<12>:"))
  (if (null z) (setq z 12))
  ;;;以下根据用户输入条件确定链轮参数初始值,即确定与链轮咧合的链条参数
  (setq no (strcase no))
  (cond ((= no "05B") (setq p 8.00) (setq dr 5.00) (setq pt 5.64) (setq b1 3.00) (setq h 7.11))
        ((= no "06B") (setq p 9.525) (setq dr 6.35) (setq pt 10.24) (setq b1 5.72) (setq h 8.26))
        ((= no "08A") (setq p 12.7) (setq dr 7.95) (setq pt 14.38) (setq b1 7.85) (setq h 12.07))
        ((= no "08B") (setq p 12.7) (setq dr 8.51) (setq pt 13.92) (setq b1 7.75) (setq h 11.81))
        ((= no "10A") (setq p 15.875) (setq dr 10.16) (setq pt 18.11) (setq b1 9.4) (setq h 15.09))
        ((= no "12A") (setq p 19.05) (setq dr 11.91) (setq pt 22.78) (setq b1 12.57) (setq h 18.08))
        ((= no "16A") (setq p 25.4) (setq dr 15.88) (setq pt 29.29) (setq b1 15.75) (setq h 24.13))
        ((= no "20A") (setq p 31.75) (setq dr 19.05) (setq pt 35.76) (setq b1 18.9) (setq h 30.18))
        ((= no "24A") (setq p 38.1) (setq dr 22.23) (setq pt 45.44) (setq b1 25.22) (setq h 36.2))
        ((= no "28A") (setq p 44.45) (setq dr 25.4) (setq pt 48.87) (setq b1 25.22) (setq h 42.24))
        ((= no "32A") (setq p 50.8) (setq dr 28.585) (setq pt 58.55) (setq b1 31.55) (setq h 48.26))
        ((= no "40A") (setq p 63.5) (setq dr 39.68) (setq pt 71.55) (setq b1 37.85) (setq h 60.33))
        ((= no "48A") (setq p 76.2) (setq dr 47.63) (setq pt 87.83) (setq b1 47.35) (setq h 72.39))
        (t                         (setq dr 7.95) (setq pt 14.38) (setq b1 7.85) (setq h 12.07))
  )
  (setq ang1 (/ pi z))  ;;;ang1为180/z的弧度值,本角度为方便下面计算而定义
  ;;;以下进行链轮参数计算
  (setq angdd (/ (* 2 pi) z))                 ;;;每个齿对应的角度
  (setq re (* 0.12 dr (+ z 2)))               ;;;齿面圆弧半径
  (setq ri (* dr 0.505))                      ;;;齿沟圆半径
  (setq a (- (* (/ pi 180) 140) (/ (* pi 0.5) z)))    ;;;齿沟角
  (setq d (/ p (sin (/ pi z))))                   ;;;分度圆直径
  (setq da (fix (/ (+ (- (+ d (* p (- 1 (/ 1.6 z)))) dr) (- (+ d (* p 1.25)) dr)) 2)))     ;;;齿顶圆直径
  (setq df (- d dr))                          ;;;齿根圆直径
  (setq dg (fix (- (* p (/ (cos ang1) (sin ang1))) (* h 1.04) 0.76 dr)))    ;;;齿侧凸缘直径
  ;;;以下进行绘图点的定义与赋值
  (setq c1 (getpoint "请输入中心点:"))    ;;;链轮中心点
  (setq c2 (polar c1 (* -1 (* angdd 0.5)) (* d 0.5)))
  (setq cr1 (polar c2 (- (+ pi (* a 0.5)) (* angdd 0.5)) (+ re ri)))
  (setq cr2 (polar c2 (- (- pi (* a 0.5)) (* angdd 0.5)) (+ re ri)))
  ;;;求三角1角度angm1
  (setq u1 (* d 0.5))
  (setq v1 (+ ri re))
  (setq w1 (distance c1 cr1))
  (setq s1 (* (+ u1 v1 w1) 0.5))
  (setq area1 (sqrt (* s1 (- s1 u1) (- s1 v1) (- s1 w1))))
  (setq h1 (/ (* 2 area1) w1))
  (setq l1 (sqrt (- (* u1 u1) (* h1 h1))))
  (setq angm1 (atan h1 l1))
  ;;;求三角2角度angm2
  (setq u2 (* da 0.5))
  (setq v2 re)
  (setq s2 (* 0.5 (+ u2 v2 w1)))
  (setq area2 (sqrt (* s2 (- s2 u2) (- s2 v2) (- s2 w1))))
  (setq h2 (/ (* 2 area2) w1))
  (setq l2 (sqrt (- (* u2 u2) (* h2 h2))))
  (setq angm2 (atan h2 l2))
  ;;;求齿顶圆弧对应的角度angm4
  (setq angm3 (- angm1 angm2))
  (setq angm4 (- angdd (* angm3 2)))
  ;;;继续进行绘图点的定义与赋值
  (setq pa1 (polar c1 (* -1 (+ (* angdd 0.5) angm3)) (* da 0.5)))
  (setq pa2 (polar c2 (- (+ pi (* 0.5 a)) (* angdd 0.5)) ri))
  (setq pa3 (polar c1 (* -1 (* angdd 0.5)) (- (* d 0.5) ri)))
  (setq pa4 (polar c2 (- (- pi (* a 0.5)) (* angdd 0.5)) ri))
  (setq pa5 (polar c1 (* -1 angm4 0.5) (* da 0.5)))
  (setq pa6 (polar c1 0 (* da 0.5)))
  (setq pa7 (polar c1 (* angm4 0.5) (* da 0.5)))
  ;;;关闭捕捉
  (setq os (getvar "osmode"))
  (setvar "osmode" 0)
  ;;;绘制轮齿
  (command "pline" pa1 "w" 0 "" "a" "ce" cr1 pa2 "s" pa3 pa4 pa5 "s" pa6 pa7 "")
  (command "array" (entlast) "" "p" c1 z "" "")
  (command "circle" c1 c2)
  (command "change" (entlast) "" "p" "la" "0" "")
  ;;;打开捕捉
  (setvar "osmode" os)
  (princ)
  )
