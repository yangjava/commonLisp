;progn ������ϣ��LISP�Ѷ�����䵱��һ����ʱ��Ҫ��(progn �������һ�� �������� �������������


(progn 
(+ 1 5)
(- 5 1)
(* 1 5)
)

;>>  5 

;�� progn �����еı���ʽ��������ֵ������������һ������ʽ��ֵ
;������ 5

(progn
(format t "a")
(format t "b")
(+ 1 2)
)

>> ab
>> 3

;���ֻ��������һ������ʽ��ֵ��������ʹ�� progn �����κ����飩��ζ�Ÿ�����
 
(progn
(+ 1 2)
(format t "a")
(format t "b")
)

>> ab
>> NIL