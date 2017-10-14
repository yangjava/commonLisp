;+-------------------------------------------+
;查找或替换字符串,字串必须完全匹配           +
;文件名：CHG-TEXT.lsp 			     +
;---------------------------------------------+
;局部查找函数
(defun find2(s os ns / st s nsl osl sl si chf)
   (setq chf nil 
         si 1
         osl (strlen os)
         nsl (strlen ns)
    ) ;setq end
    (while (= osl (setq sl (strlen
         (setq st (substr s si osl)))))
         (if (= st os)
             (setq s (strcat (substr s 1 (1- si)) ns
                                  (substr s (+ si osl)))
                   chf t ; Found old string
                   si (+ si nsl)
              ) ;setq end
              (setq si (1+ si))
          ) ;if end
     ) ;while 2
     (if chf s nil) ;如果找到，返回新字符串，否则返回nil
) ;defun end

(defun C:chg-text(/ ss txsize n index ent ty oldsize newsize ent1)
 (inivar) ;初始化系统变量
 (setq yn (getstring "\n查找(Find) 替换(Chang):<C>")
       yn (if (= yn "") "C" (strcase (substr yn 1 1)))
       mode (getint "\查找字符方式:1(完全匹配)/2(局部匹配):<1>")
       mode (if mode mode 1)
 ) ;setq end
 (setq ss (ssget))
 (if (= yn "C") 
   (setq oldtxt (getstring "\n查找字串:")
         newtxt (getstring "\n替换字串:")
         ttext "\n总共替换字串:"
   ) ;setq end
   (setq oldtxt (getstring "\n查找字串:") newtxt oldtxt ttext "\n总共找到字串:")
 ) ;if end
 (setq temp (if (= (strlen oldtxt) 0) 0 1))
 (while (= temp 1)
  (setq n (sslength ss))
  (setq index 0 index1 0 ) 
  (repeat n
   (setq entn (ssname ss index)
        ent (entget entn)
        index (+ 1 index)
        ty (assoc 0 ent)
   ) ;if end
   (if (or (= "TEXT" (cdr ty)) (= "MTEXT" (cdr ty)))
   (progn
    (setq oldsize (assoc 1 ent)
          old (cdr oldsize)  ;取字符串
          h_txt (cdr (assoc 40 ent)) ;取字符高
          wid   (cdr (assoc 41 ent)) ;取字符高宽比
          len (strlen old)           ;字符个数
    ) ;setq end
    (cond
      ((= mode 1) (setq yes (if (= old oldtxt) T Nil))) ;全部匹配
      ((= mode 2) (setq newstr (find2 old oldtxt newtxt)
                        yes (if newstr T Nil)
                        newstr (if newstr newstr old)
                   ) ;setq end
      ) ;condition end
    ) ;cond end
    (if yes
     (progn
      (setq p0 (cdr (assoc 10 ent)) ;取字符起点
            bb (* h_txt wid len 1.1) ;字符总宽度
            p1 (polar p0 pi18 bb)
            p1 (polar p1 pi27 bb)
            p2 (polar p0 0 bb)
            p2 (polar p2 pi09 bb)
       ) ;setq end
       (command "zoom" "w" p1 p2)
       (getstring "\n按<ENTER>继续查找:")
       (command "zoom" "p")
    (cond
      ((= mode 1) ;全部匹配
        (setq newsize (cons (car oldsize) newtxt)
              ent1 (subst newsize oldsize ent)
        ) ;setq end
       ) ;condition end
      ((= mode 2) ;局部匹配
        (setq newsize (cons (car oldsize) newstr)
              ent1 (subst newsize oldsize ent)
        ) ;setq end
      ) ;condition end
     ) ;cond end
      (setq index1 (1+ index1))
      (entmod ent1)
     ) ;progn end
    ) ;if end
   ) ;progn end
  ) ;if end
 ) ;repeat end
 (princ (strcat ttext (rtos index1 2 0) " 次"))
 (setq oldtxt (getstring "\n查找字串:"))
 (if (> (strlen oldtxt) 0)
  (setq temp 1
        newtxt (if (= yn "C") (getstring "\n替换字串:") oldtxt)
  ) ;if end
  (setq temp 0)
 )
) ;while end
 (resvar) ;还原系统变量
 (princ)
) ;defun enf

(princ "\n程序已装入，键入Chg-text命令开始程序。")
(princ "\nCopyRight Liu liping 2002/06/10")
(princ)