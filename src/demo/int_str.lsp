
;+-------------------------------------------+
;将数字字符自动取整到以50、00结尾            +
;程序会自动筛选出TEXT和MTEXT实体              +
;文件名：INT_str.lsp 			            +
;---------------------------------------------+
(defun C:INT_str(/ ss txsize n index ent type oldsize newsize ent1)
  (inivar) ;初始化系统变量
  (princ "\n选择物体: ")
  (setq ss (ssget)
        n (sslength ss)
        sel1 1
        index 0
  ) ;setq end
 (repeat n   ;对选中的实体逐一处理
  (setq ent (entget (ssname ss index)))   ;用entget函数取实体关系表
  (setq index (+ 1 index))
  (setq type (assoc 0 ent))    ;用assoc函数取实体类型子表
  (if (OR (= "TEXT" (cdr type)) (= "MTEXT" (CDR TYPE)) ) ;仅处理TEXT,MTEXT
   (progn
    (setq oldsize (assoc sel1 ent)   ;取出旧子表
          old_txt (cdr oldsize)
          old_num (atoi old_txt)
          new_num (* 50 (+ 1 (/ old_num 50)))  ;对数字取整到50
          new_txt (itoa new_num)
          newsize (cons (car oldsize) new_txt) ;生成新子表
          ent1 (subst newsize oldsize ent)    ;用新子表替换旧子表
          oldsize (assoc 72 ent)
          newsize (cons (car oldsize) '0)
          ent2 (subst newsize oldsize ent1)
    ) ;setq end
    (if (> old_num 300)
      (entmod ent2)  ;在图形数据库中修改实体关系表信息
    ) ;if end
   )  ;prong end
  )  ;if end
 ) ;repeat end
(resvar) ;还原系统变量
) ;defun end
