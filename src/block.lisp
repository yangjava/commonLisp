;��һ������ӦΪ���š���������������֡�
;�������е��κεط��������ֹͣ��ֵ����͸��ʹ�� return-from ָ����������֣�
;����������һ����ֵ��


(block head
(format t "Here we go.")
(return-from head 'idea)
(format t "We'll never see this."))


>> Here we go.
>> IDEA