;画板式楼梯
(defun c:lt()
  (setvar "cmdecho" 0)
  (setq txt (strcat "\n请输入楼梯型式 1:下面带基础，上面带梯梁；2:下面带梯梁，上面带平段；" 
                   "\n3:下面带平段，上面带梯梁；4:上下均带平段；5: 上下均带梯梁"
                   "\n6:三跑折楼梯:") )
 (princ txt)
 (setq n_type (getstring)
       txt_lib (strcat "lt-" (substr n_type 1 1))
 )
 (princ "\n输入图块插入点：")
 (command "insert" txt_lib  "x" 1 "y" 1 "r" 0 pause)
 (setvar "cmdecho" 1)
);defun end