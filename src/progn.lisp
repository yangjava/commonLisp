;progn 当我们希望LISP把多条语句当做一条的时候，要用(progn …（语句一） （语句二） （语句三）…）


(progn 
(+ 1 5)
(- 5 1)
(* 1 5)
)

;>>  5 

;在 progn 主体中的表达式会依序求值，并返回最後一个表达式的值
;输出结果 5

(progn
(format t "a")
(format t "b")
(+ 1 2)
)

>> ab
>> 3

;由於只返回最後一个表达式的值，代表著使用 progn （或任何区块）意味着副作用
 
(progn
(+ 1 2)
(format t "a")
(format t "b")
)

>> ab
>> NIL