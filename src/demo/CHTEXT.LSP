;+-------------------------------------------+
;修改字符高度、字型、高宽比等属性            +
;程序会自动筛选出TEXT和MTEXT实体           +
;文件名：CHTEXT1.lsp 			    +
;---------------------------------------------+
(defun C:chtext(/ ss txsize n index ent ty oldsize newsize ent1)
  (setq oer *error* *error* clmerr)  ;错误处理机制
  (setvar "cmdecho" 0)
  (setq osold (getvar "osmode"))
  (setvar "osmode" 0)
  (princ "\n选择物体: ")
  (setq ss (ssget))
 (setq ll '(40 41 7)
       sel (getstring "\nchange Heiht/Wide/Style/Angle:")
 ) ；setq end
(cond
 ((or (= sel "H") (= sel "h"))  ;如果改变字高
    (setq txsize (getdist "\n输入新的文本高:")
          sel1 40    ;TEXT实体字高组码为40
    ) ;setq end
 ) ;条件1
 ((or (= sel "W") (= sel "w")) ;如果改变字高宽比
    (setq txsize (getdist "\n输入新的文字高宽比:") 
         sel1 41    ;TEXT实体高宽比组码为41
    ) ;setq end
 );条件2
 ((or (= sel "S") (= sel "s"))  ;如果改变字型
    (setq txsize (getstring "\n输入新的文字字型:")
        sel1 7   ;TEXT实体字型组码为7
    ) ;setq end
 );条件3
 ((or (= sel "A") (= sel "a"))
    (setq txsize (getreal "\n输入新的文字角度(单位度):")
            sel1 50
            txsize (* pi txsize (/ 1.0 180)) ;角度转化成弧度
    ) ;setq end
  );条件4
 ) ;cond end
(setq n (sslength ss)
index 0
) ;setq end
 (repeat n   ;对选中的实体逐一处理
  (setq ent (entget (ssname ss index)))   ;用entget函数取实体关系表
  (setq index (+ 1 index))
  (setq ty (assoc 0 ent))    ;用assoc函数取实体类型子表
  (if (OR (= "TEXT" (cdr ty)) (= "MTEXT" (cdr ty)) ) ;仅处理TEXT,MTEXT
   (progn
    (setq oldsize (assoc sel1 ent)   ;取出旧子表
          newsize (cons (car oldsize) txsize) ;生成新子表
          ent1 (subst newsize oldsize ent)    ;用新子表替换旧子表
          oldsize (assoc 72 ent)
          newsize (cons (car oldsize) '0)
          ent2 (subst newsize oldsize ent1)
    ) ;setq end
    (entmod ent2)  ;在图形数据库中修改实体关系表信息
   )  ;prong end
  )  ;if end
 ) ;repeat end
 (setvar "cmdecho" 1)
 (setvar "osmode" osold)
) ;defun end

(defun c:pmfh()
 (setvar "cmdecho" 0)
 (command "layer" "s" "rei" "")
 (setvar "cmdecho" 1)
 (command "line" pause pause "")
 (command "line" pause pause "")
 (setvar "cmdecho" 0)
 (command "layer" "s" "0" "")
 (setvar "cmdecho" 1)
 (setq str-n (getstring "\nsetion number:")
       p1 (getpoint "\np1:")
       p2 (getpoint "\np2:")
 ) ;setq end
 (command "text" p1 pause pause str-n)
 (command "text" p2 "" "" str-n)
) ;defun end
(defun c:las()
 (setvar "cmdecho" 1)
 (command "layer" "s" pause "")
) ;defun end
