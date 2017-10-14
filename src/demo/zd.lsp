;;****************չ��  ������******************
(defun c:zd( / luj oldosmode mjrow)
;;(if (null cal)(arxload"geomcal"))
;;(princ "\n �������ļ�.....")
  (setvar "cmdecho" 0)
  (setvar "DIMZIN" 1)
  (setq oldosmode (getvar "osmode"))
  (setvar "osmode" 0)
  (if lu_jing T (setq lu_jing "e:/"))

  (setq luj (getfiled "���ļ�..." lu_jing "txt" 2))
  (if luj   (progn 
               (setq mjrow(Wfchli luj) lu_jing luj)
               (Fcld mjrow))
   (princ "\n �ļ���ȡ���������˳���"))
   (setvar "osmode" oldosmode)
   (princ)
)


(defun Fcld( cctv / qss)
(while (progn
      (initget "P C ")
      (setq qss(getpoint "\n ƽ��ͼչ��(P) / �����չ��(C) /<�˳�>��")) 
      
      (if (= qss "P") (progn
                        ;;  չ������ƽ��ͼ
                        (command "pline")
                        (mapcar 'command (Coorde cctv))
                        (command "")
                        (command "undo" "be")
                        (Xtext cctv)
                        (command "undo" "e")
                        (command "zoom" "e")) 
                       )
      (if (= qss "C") (progn 
                        ;;  ������չ����������ͼ
                        (if (findfile "dmtk.dwg")
                        (command "insert" "dmtk.dwg" "0,0" "" "" "")
                        (princ "\n δ�ҵ���߼�ͼ����ļ�"))
                        (command "pline")
                        (mapcar 'command (Hlist cctv))
                        (command "") 
                        (command "undo" "be")
                        (Hline cctv) ;;��ע���뼰�߳�
                        (command "undo" "e")
                        (command "zoom" "e"))
                )
                qss
  ))
)


;;******************��ע�߳�*********************
(defun Xtext(mja / n h zaa xya)
(setq n 0 h "10" ;;���ָ߶�
      zaa (Coordz mja)
      xya (Coorde mja))
(repeat (length zaa)
   (command "text" (nth n xya) h "0" (rtos (nth n zaa) 2 2))
   (setq n (1+ n)))
)


;;*************���������뼰�߳�****************
(defun Hlist(mjh / n h zaa xya)
(setq xx 1 yy 1) ;;������
(setq n 0 HL 0 hct '()
      zaa (Coordz mjh)
      xya (Coorde mjh))
(repeat (- (length zaa) 1)
   (if (/= (nth n zaa) 0)
   (setq hct (cons (list (* HL xx) (* (nth n zaa) yy)) hct))) 
   
   (setq HL  (+ hl (distance (nth n xya) (nth (+ n 1) xya)))
          n (1+ n)))
   (reverse hct)
)


;;***********�ں����ϱ�����뼰�߳�**************
(defun Hline(mjn / n h yl yml xl xml) ;;
(setq xx 1 yy 1) ;;������
(setq csa 30  ;;�������߼�ľ���
      csd 5  ;;��ע���ߵĳ���
      csh "5" ;;��ע�ָ�
  )
 (setq n 0 xp (Hlist mjrow)
       h (length xp)
       xl (car (nth 0 xp))
       xml (car (nth (- h 1) xp))
       yl (- (cadr (nth 0 xp)) 114)
       yml (- yl csa)
)
       
 (command "line" (list xl yl) (list xml yl) "")
 (command "line" (list xl yml) (list xml yml) "")
 (repeat h 
  (setq xc (car (nth n xp)))
  (command "line" (list xc yml)(list xc (+ yml csd)) "")
  (command "text" "j" "ml" (list xc (+ yml (* 1.2 csd)))
    csh "90" (rtos (/ xc xx) 2 1))
  (setq hc (cadr (nth n xp)))
  (command "line" (list xc yl)(list xc (+ yl csd)) "")
  (command "text" "j" "ml" (list xc (+ yl (* 1.2 csd)))
   csh "90" (rtos (/ hc yy) 2 1))
  (setq n (1+ n))
)


)

;;******************��ȡ�߳�*********************
(defun Coordz(mj / n aa ccx) 
  (setq n 0 aa '())
   (repeat (length mj)
   (setq ccx (nth n mj) n (1+ n)
         aa (cons (nth 3 ccx) aa)
   )) (reverse aa)
)



;;******************��ȡ����*********************
;;�����ά�� (��� x���� y���� �߳�)
;;����ÿ��Ԫ�ص���ά����
(defun Coorde(mj / n aa ccx) 
  (setq n 0 aa '())
   (repeat (length mj)
   (setq ccx (nth n mj) n (1+ n)
         aa (cons (list (nth 1 ccx)(nth 2 ccx)(nth 3 ccx)) aa)
   )) (reverse aa)
)

;;***********�Ѷ����ַ��ļ�ת��Ϊ��**************
;;��Ҫtt_fk�ӳ����֧��
;;�����ļ���ȫ������·�����ַ���
;;����ֵΪ��ά��
(defun Wfchli(lcc / f_dat txt_row ccdate) 
 (setq ccdate '() f_dat (open lcc "r"))
    (while (setq txt_row (read-line f_dat))
    (if (> (strlen txt_row) 1)
    (setq ccdate (cons (Tt_fk txt_row) ccdate)))
) (reverse ccdate)
)



;;***********��һ���ַ���ת��Ϊ��****************
;;�����ַ��������� "1,453.34,452.63,53.3"
;;����ֵΪһ���� (1 453.34 452.63 53.3)
(defun Tt_fk(tt / k tr_k nn)  
 (setq nn (strlen tt) k 0 tr_k "")
 (while (<= k nn)
        (setq tr_kn (substr tt (setq k (+ k 1)) 1))
        (if (or (= tr_kn ",")(= tr_kn ";"))
        (setq tr_k (strcat tr_k " "))
        (setq tr_k (strcat tr_k tr_kn)))
 )(read (strcat "(" tr_k ")"))
)



;;Ӧ�ι���Ҫ���д������
;;������  2005.11.3
