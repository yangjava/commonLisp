(defun c:tong()
 (setvar "cmdecho" 0)
 (princ "\n输入表格插入点:")
 (setq bl (getvar "userr1")
       l1 (* 260 bl)
       l2 (* 60 bl)
 ) ;setq end
 (command "insert" "border" "x" l1 "y" l2 "r" 0 pause)
 (command "erase" "l" "")
 (setq p1 (getvar "lastpoint")
       p2 (polar p1 0 l1)
       p2 (polar p2 pi09 l2)
  ) ;setq end
  (command "insert" "huncs" p1 bl bl "")
  (command "zoom" "w" p1 p2)
  (getstring "\n按任意键继续：")
  (command "erase" "l" "")
  (command "zoom" "p")
) ;defun end
