;;��������
;;============================================================================
;;1.ͼ�ţ�J50H033484
;;2.�ܼ���
;;3.�����߷�ĸ��5000
;;4.����ͼ���ǵ�X���꣺ 4394532.3
;;5.����ͼ���ǵ�Y���꣺ 486592.5
;;6.����ͼ���ǵ�X���꣺ 439445.4
;;7.����ͼ���ǵ�Y���꣺ 4865841.2
;;9.����ͼ���ǵ�Y���꣺ 489377.3
;;10.����ͼ���ǵ�X���꣺ 4342528.1
;;11.����ͼ���ǵ�Y���꣺ 483274.0
;;12.DOM���з�Χ���Ͻǵ�X���꣺4354895
;;13.DOM���з�Χ���Ͻǵ�Y���꣺483427
;;14.DOM���з�Χ�����ǵ�X���꣺4367895
;;15.DOM���з�Χ�����ǵ�Y���꣺486542
;;16.DOM���з�Χ�����ǵ�X���꣺4378478
;;17.DOM���з�Χ�����ǵ�Y���꣺498542
;;18.DOM���з�Χ���Ͻǵ�X���꣺4348478
;;19.DOM���з�Χ���Ͻǵ�Y���꣺480627
;;20.��ҵԱ��
;;21.���Ա��
;;22.������������79MB
;;============================================================================
;��txt�ļ��������������lisp����.�������ƻ����ȷ�һ��Ҳ���ܰ��ѧ��һ��æ.->
;����һ���й�DMC��׼ǰ��txt��(������)���ض��ַ����������С����,��������ֻ��ȡ���ϺͶ����ǵ�;��X,Y����.���Ǽ����˼����˵�˼·��д��.��������,���������������ĳ�ѧ����˵���ܻ����һ��æ;��.���Ծ��ȷ�����һ��.ϣ�����ᱦ�����.
;================================================================
(defun c:DMCpz ( / txt fi start li str_tfh str_blc
  str_WSx str_WSy str_ENx str_ENy WS_x WS_y EN_x EN_y
        )
  
   (setq path_txt (getfiled "Select .txt file" "" "txt" 2))
   (setq fi (open path_txt "r"))
  
   (setq ;str_tfh ""
 str_WSx ""
 str_WSy ""
 str_ENx ""
 str_ENy ""
 ;str_blc "1:5000"
 )
  
  (setq start T)
  
  (while (and start (setq li (read-line fi)))
    (cond
      ((wcmatch li "*ͼ��*") (setq str_tfh li))
      ((wcmatch li "*DOM���з�Χ���Ͻǵ�X����*") (setq str_WSx li))
      ((wcmatch li "*DOM���з�Χ���Ͻǵ�Y����*") (setq str_WSy li))
      ((wcmatch li "*DOM���з�Χ�����ǵ�X����*") (setq str_ENx li))
      ((wcmatch li "*DOM���з�Χ�����ǵ�Y����*") (setq str_ENy li)(setq start nil))
      (t nil)
    )
  )
    (setq WS_x
      (atof (substr str_WSx (+ 1 (strlen "��") (vl-string-search "��" str_WSx))) 
             ))
    (setq WS_y
      (atof (substr str_WSy (+ 1 (strlen "��") (vl-string-search "��" str_WSy))) 
             ))
    (setq EN_x
      (atof (substr str_ENx (+ 1 (strlen "��") (vl-string-search "��" str_ENx))) 
             ))
    (setq EN_y
      (atof (substr str_ENy (+ 1 (strlen "��") (vl-string-search "��" str_ENy))) 
             ))
   
     (command ".line" (list WS_x WS_y) (list EN_x EN_y) "")
       
    (close fi)
  (princ)
)

