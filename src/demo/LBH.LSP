;+------------------------+
;文件名: LBH.lsp          +
;+------------------------+
;判别字符变量text中如包含字符变量txt_f,返回所在的位置,否则返回0
(defun iiif(text txt_f)
 (setq  num  1)
  (while (and (<= num (strlen text))
              (not (= (substr text num 1) txt_f)) 
         ) ;and end
      (setq num (+ num 1)) ;setq end   
  ) ;while end
  (if (<= num (strlen text)) num 0)
) ;defun end

(defun C:lbh(/ ss txsize n index ent ty oldsize newsize ent1 ind1)
 (inivar) ;初始化系统变量
 (princ "\n选择梁编号:")
 (setq ss (ssget)
       n (sslength ss)
       index 0
       ind1  0
 ) ;setq end
 (command "style" "rmc" "simplex,hztxt" 0 0.7 0 "N" "N") ;设置字型
 (princ "\n正在处理，请稍候......")
 (repeat n
  (setq ent (entget (ssname ss index)) ;获取实体关系表
        entname (ssname ss index)      
        index (+ 1 index)
        ty (cdr (assoc 0 ent))
  ) ;setq end
  ;判别是否含有字符"L"的字符串
  (if (and (= ty "TEXT") (> (iiif (cdr (assoc 1  ent)) "L") 0))  ;if 1
    (progn  ;progn 1
     (setq la   (cdr (assoc 8 ent))  ;层名
           alfa (cdr (assoc 50 ent)) ;字符角度
           h    (cdr (assoc 40 ent)) ;字符高度
           p1   (cdr (assoc 10 ent)) ;字符起点
           txt  (cdr (assoc 1  ent)) ;字符文本
           txt1 (substr txt 2 1)
           num  (iiif txt " ")  ;查找空格
     ) ;setq end
    (if (> num 0) ;if 2
     (progn ;progn 2
      (setq t2 (substr txt 1 (1- num)) ;把字符串按空格前后分成两个
            t1 (substr txt (+ num 1))
            p2 (polar p1 (+ alfa pi09) (+ h (* 0.4 h)))
            alfa (/ (* alfa 180) pi)  ;弧度转角度
            ind1 (1+ ind1)
      ) ;setq end
      (entdel entname) ;删除原来字符
      (command "layer" "s" la "")  ;切换到字符串所在的层
      (command "text" p1  h alfa t1) ;重新写字符串
      (command "text" p2  h alfa t2)
     ) ;progn 2
    ) ;if2 end
   ) ;progn1 end
  ) ;if1 end
 ) ;repeat end
 (princ (strcat "\n共处理梁编号 " (rtos ind1 2 0) "个"))
 (resvar) ;还原系统变量
 (princ)
) ;defun end
