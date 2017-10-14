;+-------------------------------------------+
;字符对齐                                    +
;文件名：ALI-TXT.lsp       		     +
;--------------------------------------------+
;计算字符宽度
(defun mylen(ss / index nn1 nn2 txt )
   (setq index 1 
         nn1 (strlen ss)
         nn2 0)
   (while (<= index nn1)
        (setq txt (substr ss index 1))
        (cond 
          ((= txt "1")  ;如果为字符"1",只算半个字符宽
             (setq index (1+ index) 
                   nn2 (+ nn2 0.5) 
             ) ;setq end
           ) ;condition 1
          ((> (ascii txt) 134)  ;如果为汉字,两个字符算一个字符宽
             (setq index (+ 2 index) 
                   nn2 (+ nn2 1) 
             ) ;setq end
           ) ;condition 2
          (T (setq index (1+ index) nn2 (1+ nn2)))
         ) ;cond end
     ) ;while end
     (setq nn2 nn2) ;返回nn2
) ;defun end

;(defun C:ali-txt(/ ss txsize n index ent ty oldsize newsize ent1)
(defun C:ali-txt()
  (inivar) ;初始化系统变量
  (princ "\n选择物体: ")
  (setq ss (ssget)
        p0 (getpoint "\n请指定对齐点:")
        mode (getint "\n对齐方式:左对齐(1)/右对齐(2)/上对齐(3)/下对齐(4)/中对齐(5):")
        n (sslength ss)
        index 0
  ) ;setq end
  (repeat n   ;对选中的实体逐一处理
   (setq ent (entget (ssname ss index)))   ;用entget函数取实体关系表
   (setq index (+ 1 index))
   (setq ty (assoc 0 ent))    ;用assoc函数取实体类型子表
   (if (OR (= "TEXT" (cdr ty)) (= "MTEXT" (cdr ty)) ) ;仅处理TEXT,MTEXT
    (progn
     (setq old_sub (assoc 10 ent)   ;取出字高旧子表
           p_old (cdr old_sub)
           h_txt (cdr (assoc 40 ent))      ;取字高
           w     (cdr (assoc 41 ent)) ;取字高宽比
           txt_txt (cdr (assoc 1 ent)) ;取字符串
           len_txt (mylen txt_txt)    ;计算字符长度
     ) ;setq end
     (cond
       ((= mode 1) ;左对齐
             (setq ali_p (list (nth 0 p0) (nth 1 p_old)))
       ) ;;左对齐
       ((= mode 2) ;右对齐
             (setq l_txt (* w len_txt h_txt)  ;计算字符总宽度
                   p1 (polar p0 pi18 l_txt) 
                   ali_p (list (nth 0 p1) (nth 1 p_old))
             ) ;setq end
       ) ;右对齐
       ((= mode 3) ;上对齐
           (setq p1 (polar p0 pi27 h_txt)
                 ali_p (list (nth 0 p_old) (nth 1 p1))
           ) ;setq end
       ) ;上对齐
       ((= mode 4) ;下对齐
             (setq ali_p (list (nth 0 p_old) (nth 1 p0)))
       ) ;下对齐
       ((= mode 5) ;中对齐
             (setq l_txt (* 0.5 w len_txt h_txt)  ;计算字符总宽度的一半
                   p1 (polar p0 pi18 l_txt) 
                   ali_p (list (nth 0 p1) (nth 1 p_old))
             ) ;setq end
       ) ;中对齐
     ) ;cond end
     (setq new_sub (cons (car old_sub) ali_p) ;生成新子表
           ent1 (subst new_sub old_sub ent)    ;用新子表替换旧子表
     ) ;setq end
     (entmod ent1)  ;在图形数据库中修改实体关系表信息
    )  ;prong end
   )  ;if end
  ) ;repeat end
 (resvar) ;还原系统变量
 (princ)
) ;defun end
