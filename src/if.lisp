;C语言中最常见的条件分支结构莫过于if语句了。
if语句是一个典型的开关结构或叫二选一结构，即if后面的条件成立，执行一个分支；
否则执行另外一个分支。其典型结构如下：

if (cond expr) {
    … …
} else if (cond expr) {
    … …
} else if (cond expr) {
    … …
} else {
    … …
}

Common Lisp中也有if。
与progn一样，Common Lisp中的if也是一个special operator而不是函数。
函数的原则是必须对所有参数都进行求值，且对每个参数仅进行一次求值；
而if和progn则不一定需要对所有"参数"进行求值。Common Lisp中if的语法形式如下：
(if cond-form
    then-form
    [else-form])

Common Lisp中的if首先对cond-form进行求值，如果为真，则对then-form求值，并将结果返回；
否则返回else-form的求值结果。如果没有else-form分支，则返回nil。
这与C语言中的条件表达式："condition_expression ? then_expression : else_expression"甚为相似。
下面是一个例子：


(if (> 3 2) 
	(+ 4 5) 
	(- 11 3)
)
>> 9

(if (< 3 2) 
	(+ 4 5) 
	(- 11 3)
)

>> 8


(if (< 3 2)
	(+ 4 5)
 )
>> NIL


 (if (= 2 2)  ;; if级联示例
        (if (> 3 2) 
		4 
		6)
      9)
	  
	  
>> 4
	  

(if (zerop 0)
0
-1
)

>> 0

























除了if，Common Lisp还提供了其他一些简便实用的条件分支控制operator。

我们常常会在某个条件分支中顺序地执行多个表达式，这种情况下，我们用if实现的代码如下：
(if (cond-form)
    (progn
        (form1)
        (form2)
        (form3)))

Common Lisp提供了操作符when来应对如此需求，并简化你的代码：
(when (cond-form)
        (form1)
        (form2)
        (form3))

当cond-form求值为真时，when会顺序从form1执行到form3。

Common Lisp还提供了unless，用于否定语义的判断：
(unless (cond-form)
        (form1)
        (form2)
        (form3))

仅当cond-form求值为nil时，form1到form3才会被顺序执行，否则返回nil。

我们日常还会遇到条件分支特别多的情况，如：
if (cond-1)
    statments-1
if (cond-2)
    statments-2
… …
if (cond-n)
    statments-n

此时如果用if来实现，代码就显得层次太深，不够简洁，可读性不好，也难于后续维护：
(if (cond-1)
    (statments-1)
    (if (cond-2)
        (statments-2)
        …..
            (if (cond-n)
                (statments-n))))

Common Lisp提供了cond操作符来应对这一情况：

(cond
    ((cond-1) (statments-1))
    ((cond-2) (statments-2))
    … …
    ((cond-n) (statments-n)))

C语言中还有一种分支结构switch…case，可用于将一个变量与诸多常量相比较。变量与哪个case中的常量相等，就继续执行该case所在的分支代码。有些资料中将该结构称为选择结构，这里我把它统一划归在条件分支一类中。因为只有满足case条件，执行权才会进入到这个分支：
switch (expression) {
    case (const expression):
        statments;
        … …
    case (const expression):
        statments;
    default:
        statments;
}

Common Lisp中也有与switch…case对应的结构：case。
[1] > (defun grade-meaning (grade)
        (case grade
            ((5) "Excellent")
            ((4) "Good")
            ((3) "Average")
            ((2) "Poor")
            ((1) "Failing")
            (otherwise "Illegal grade")))
GRADE-MEANING
[2]> (grade-meaning 5)
"Excellent"
[3]> (grade-meaning 1)
"Failing"
[4]> (grade-meaning 0)
"Illegal grade"

case结构中的otherwise类似与C语言中switch…case中的default分支，用于处理默认情况。我们也可以用t代替otherwise，其语义是一样的：
[5] > (defun grade-meaning (grade)
        (case grade
            ((5) "Excellent")
            ((4) "Good")
            ((3) "Average")
            ((2) "Poor")
            ((1) "Failing")
            (t "Illegal grade")))

三、循环结构
和前两种控制结构相比，循环结构相对更加复杂一些。C语言提供了三种循环结构：for，do-while和while。而在Common Lisp中最通用也最灵活的循环结构为do宏。

do宏的语法形式如下：
(do ((var init-form step-form)*)
    (end-test-form result-form*)
  statement*)

和C语言中的for语句相似，do宏的执行过程也比较复杂：
1) 在初始化阶段，即循环未开始前，init-form被求值，求值结果赋给var；
2) 求值end-test-form，如果为nil，则进入子循环体，执行statement*; 如果为真，则求值result-form，并将求值结果作为do的返回值，循环结束;
3) 每个子循环执行完毕后，都会求值step-form，并用求值结果更新var；
4) 重复执行步骤2)

我们用个例子来分析一下这个执行过程，下面是一个求0到2的累加和的例子：
(do ((i 0 (1+ i))
     (sum 0 (+ sum i)))
   ((> i 2) sum))

1) 初始化：i = 0, sum = 0
2) 求值end-test-form，判断终止条件是否成立，(> 0 2)为nil，进入子循环;
3) 循环体为空，求值step-form，即i <- 0 + 1，结果i = 1; sum <- sum + i = 0 + 0(注意：这里的i用的是更新前的旧值)，结果sum = 0; 
4) 求值end-test-form，判断终止条件是否成立，(> 1 2)为nil，进入子循环;
5) 循环体为空，求值step-form，即i <- 1 + 1，结果i = 2; sum <- sum + i = 0 + 1 = 1; 
6) 求值end-test-form，判断终止条件是否成立，(> 2 2)为nil，进入子循环;
7) 循环体为空，求值step-form，即i <- 2 + 1，结果i = 3; sum <- sum + i = 1 + 2 = 3; 
8) 求值end-test-form，判断终止条件是否成立，(> 3 2)为t，求值result-form，即sum = 3，do循环结束，返回值3。

do宏通用性强，但语法及行为复杂。为了简化代码，方便使用，针对两种常见情况Common Lisp基于do宏又提供了dotimes和dolist两个宏。

dotimes宏顾名思义，适用于多次重复执行某个动作，其语法形式：
(dotimes (var max-count-form)
  body-form*)

其执行流程照比do宏要简单的多，注意max-count-form求值结果必须为一数值：
1) var初始化为0
2) 检查循环结束条件：如果var小于max-count-form的求值结果，则求值body-form；否则返回nil
3) var <- var + 1
4) 重复执行步骤2)

例如：
[1] > (dotimes (i 2) (print i))
0
1
NIL

dolist宏适用于迭代处理一个list中的诸多元素，其语法形式如下：
(dolist (var list-form)
  body-form*)

其执行流程大致如下：
1) var初始化为list-form的第一个元素
2) 检查循环结束条件：如果var不为nil，则求值body-form；否则返回nil
3) var被赋值为list-form中的下一个元素
4) 重复执行步骤2)

例如：
[1]> (dolist (i '(1 2 3))
        (print (* 2 i)))
2
4
6
NIL

[2]> (defun integer-list-sum (x)
        (let ((sum 0))
            (dolist (i x)
                (setf sum (+ sum i)))
            (print sum)))
INTEGER-LIST-SUM
[3]> (integer-list-sum '(1 2 3 4))
10

在C语言中，我们可以通过break从循环中主动退出。Common Lisp同样也提供了"break"特性，不过Common Lisp用的是return，例如：

[1]> (do ((n 0 (1+ n))
          (cur 0 next)
          (next 1 (+ cur next)))
       ((= 10 n) cur)
       (if (oddp cur)
           (progn
               (print cur)
               (return))))
1
NIL

有了三种控制结构，我们就可以用Common Lisp编写出更加富有表现力的实用代码了。以上只是Common Lisp提供的标准控制结构。别忘了，Common Lisp可是一门可编程的编程语言，我们完全可以根据自己的需要定义出更加简洁方便的控制结构，不过这是高级话题了。等我们学到宏的时候再考虑这些吧。现在的首要任务就是熟练掌握这些基本的控制结构^_^。