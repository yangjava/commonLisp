; texth-ch.Lsp
; �޸��ָ�
; Jun 25th 2001

; ent ent_data ent_lay_new ent_lay_name ent_list ent_list_len
; id ent_lay_old ent_data_new msg1 msg2

(defun c:llsp()
  (load "tch")
)

(defun c:tch(/ orig_blip orig_cmd ent nt_data ent_lt_new 
		    ent_list ent_list_len id ent_lt_old
		    ent_data_new msg1 msg2)
  (setq orig_blip (getvar "blipmode"))
  (setq orig_cmd (getvar "cmdecho"))
  (setvar "blipmode" 0 )
  (setvar "cmdecho" 0 )

 (setq ent (entsel "\n ��ѡȡҪ�޸ĳ��ָߵ��֣�"))

  (setq ent_data (entget (car ent)))
  (setq ent_lt_new (assoc 40 ent_data))

  (prompt "\n ��ѡȡ���޸ĵ��֣� ")
  (setq ent_list (ssget))   ;ѡȡһ���޸ĵ�����,������������
  (setq ent_list_len (sslength ent_list))�����������з���Ҫ��Ķ������Ŀ
  (setq id 0)
  (repeat ent_list_len
    (setq ent_data (entget (ssname ent_list id)))  ;������ȡ��һ������
    (setq ent_lt_old (assoc 40 ent_data))  ;��ͼԪ���ָ�ֵ����һ����
    (setq ent_data_new (subst ent_lt_new ent_lt_old ent_data))���޸�ͼԪ�ָ�
    (entmod ent_data_new)
    (setq id (1+ id))
   ) ; end of repeat

  (setq msg1 "\n �ѽ�ѡȡ���ָı����ָ� ")
  (setq msg2 " ��")
  (setq msg1 (strcat msg1 msg2))
  (prompt msg1)

  (setvar "blipmode" orig_blip)
  (setvar "cmdecho" orig_cmd)
  (prin1)
 ); end of Tch

	(setq msg_ "\n .........Command: tch .........\n")
	(prompt msg_)