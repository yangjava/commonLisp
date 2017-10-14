;+-------------------------------------+
;+将钢筋符号以外的字符字型变为Rmc      +
;+-------------------------------------+
;判别字符变量text中如包含字符变量txt_f,返回所在的位置组成的表,否则返回空表
(defun find_str(text txt_f / num l_n)
 (setq  num  1 
        l_n '())
  (while (<= num (strlen text))
       (if (= (substr text num 1) txt_f)
          (setq l_n (cons num l_n)) ;如果找到字符txt_f,将num加入表l_n
       ) ;if end
      (setq num (+ num 1)) ;setq end   
  ) ;while end
  (setq l_n (reverse l_n))
) ;defun end


(defun C:chsty(/ ss txsize n index ent ty oldsize newsize ent1 n1 n2 nn)
 (inivar) ;初始化系统变量
 (setq ss (ssget)
       n (sslength ss)
       index 0
       asc1 (chr 130) ;I级钢
       asc2 (chr 131) ;I级钢
 ) ;setq end
 (princ "\n正在处理,请稍候........")
 (command "style" "rmc" "simplex,hztxt" 0 0.7 0 "N" "N") ;设置字型
 (command "style" "hz" "txt,hztxt" 0 0.7 "0" "N" "N")
 (repeat n
  (setq entn (ssname ss index)
        ent (entget entn)
        index (1+ index)
        ty (assoc 0 ent)
  ) ;setq end
  (if (OR (= "TEXT" (cdr ty)) (= "MTEXT" (cdr ty)) ) ;if1
   (progn ;progn 1
    (setq oldtxt (assoc 1 ent) ;获取字符串
          txt (cdr oldtxt)
          oldpp (assoc 10 ent) ;字符起点
          oldp (cdr oldpp)  
          ang (cdr (assoc 50 ent)) ;字符角度
          h (cdr (assoc 40 ent))   ;字符高
          la (cdr (assoc 8 ent))   ;字符所在层
          ang1 (* ang 180 (/ 1.0 pi)) ;弧度转角度
          wid (cdr (assoc 41 ent))    ;字符高宽比
          n1 (find_str txt asc1)      ;查找I级钢字符
          n2 (find_str txt asc2)      ;查找II级钢字符
          nn (if (> (length n1) 0) n1 n2)
    ) ;setq end
    (if (or (> (length n1) 0) (> (length n2) 0)) ; if2
       ;找到钢筋字符
      (progn ;progn 2
       (entdel entn) ;删除字符
       (setq indd 0)
       (command "layer" "s" la "")
       (repeat (length nn)   ;repeat2 
         (setq nnn (nth indd nn))
         (cond
           ((= nnn 1) (setq txt1 "")) ;setq end
           ((= indd 0) (setq txt1 (substr txt 1 (1- nnn))))
           ((> indd 0) 
             (setq nnn1 (nth (1- indd) nn)
                   nnn1 (1+ nnn1)
                   txt1 (substr txt nnn1 (- nnn nnn1))
             ) ;setq end
           ) ;condition end
          ) ;cond end
          (setq txt2 (substr txt nnn 1))
          (setq indd (1+ indd)
                p1 oldp
                p2 (polar oldp ang (* wid h (strlen txt1)))
                p3 (polar p2 ang (* wid h))
          ) ;setq end
          (command "style" "rmc" "" "" "" "" "" "")
          (command "text" p1 h ang1 txt1)
          (command "style" "hz" "" "" "" "" "" "")
          (command "text" p2 h ang1 txt2)
          (setq oldp p3) 
       ) ;repeat2 end
         (setq txt (substr txt (1+ nnn)))
         (command "style" "rmc" "" "" "" "" "" "")
         (command "text" p3 h ang1 txt)
       ) ;progn 2 end
       ;没找到钢筋字符,修改字符字型
       (progn  ;progn 3
          (setq oldsty (assoc 7 ent) ;取字符字型
                newsty (cons (car oldsty) "RMC")
                ent1 (subst newsty oldsty ent)
                oldsize (assoc 72 ent1)
                newsize (cons (car oldsize) '0)
                ent1 (subst newsize oldsize ent1)
          ) ;setq end
          (entmod ent1) ;修改实体
       ) ;progn 3 end
     ) ;if2 end
   ) ;progn 1 end
  ) ;if1 end
 ) ;repeat end
 (resvar) ;还原系统变量
 (princ)
)  ;defun end
