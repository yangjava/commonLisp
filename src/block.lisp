;第一个参数应为符号。这变成了区块的名字。
;在主体中的任何地方，你可以停止求值，并透过使用 return-from 指定区块的名字，
;来立即返回一个数值：


(block head
(format t "Here we go.")
(return-from head 'idea)
(format t "We'll never see this."))


>> Here we go.
>> IDEA