;���óߴ��עϵͳ����
(defun setdim(dlfac / a bl scale blx)
   (Setq bl (getvar "USERR1")
         scale bl blx (/ bl 100.0))
   (setvar "DIMDLE" 0) 
   (setvar "DIMEXO" 0) ;���������ƫ��ԭ��ľ���
   (setvar "DIMCLRT" 256) ;�ߴ��ע�ı���ȱʡ��ɫΪBYLAYER
   (setvar "DIMCLRD" 256) ;�ߴ��ע�ߡ���ͷ�顢�����ߵ�ȱʡ��ɫΪBYLAYER
   (setvar "DIMCLRE" 256) ;�ߴ������ߵ�ȱʡ��ɫΪBYLAYER
   (setvar "DIMGAP" bl) ;��ע�ı����ע�ߵľ���
   (setvar "DIMDEC" 0)  ;Primary Units��ע�ı�С����λ��
   (setq a (getvar "VIEWCTR"))
   (command "insert" "ccx" a ".001" ".001" "0") ;�����,Ȼ��ɾ��
   (command "erase"  "L" "")
   (setvar  "BLIPMODE" 1)
   (setvar "DIMBLK" "ccx") ;ָ����ͷ������
   (setvar "DIMTSZ" 0) ;����ָ���ļ�ͷ��
   (setvar "DIMSAH" 0) ;��ʾDIMBLKָ���ļ�ͷ��
   (setvar "DIMCEN" (* bl 2))
   (setvar "DIMDLI" 0) 
   (setvar "DIMEXE" (* bl 1.5)) ;�趨��ע�����������ע�ߵĳ���
   (setvar "DIMTXT" (* bl 2.5)) ;�趨��ע�ı��߶�
   (setvar "DIMDLE" 0) ;�趨��ϵͳ����DIMBLK="_OBLIQUE" ʱ�ߴ��ע����������ߵľ���
   (setvar "DIMSHO" 0) ;�϶���ر�עʱ������
   (setvar "DIMTIH" 0) ;��ע�ı����ע��ƽ��
   (setvar "DIMTOH" 0) ;��ע�ı����ע��ƽ��
   (setvar "DIMTIX" 1) ;���ǽ���ע�ı�����������֮��
   (setvar "DIMTAD" 1) ;��ע�ı������������м�
   (setvar "dimtvp" 1) ;�趨��ע�ı��ڱ�ע��֮�ϻ��µ�λ��
   (setvar "DIMSOXD" 0) ;���ǻ��Ʊ�ע��ĩ�˼�ͷ��
   (setvar "DIMASZ" (* bl 1)) ;ָ�����˼�ͷ��ͼ��Ĵ�С
   (setvar "DIMTOFL" 1) ;���ǻ��Ʊ�ע��
   (setvar "DIMTMOVE" 2) ;��TEDIT����ʱ�����ע�ı������ƶ����Ҳ�Ϊ�ı���������
   (setvar "DIMLFAC" dlfac) ;�趨�ߴ��߷Ŵ����
   (setvar "DIMTXSTY" "RMC") ;�趨��ע�ı�����
   (princ)
) ;defun end

;define new dimention style,you MAY or NOT use this dimention style
(defun c:md ( / a )
   (inivar) ;��ʼ��ϵͳ����
   (command "style" "RMC" "simplex,hztxt" 0 0.7 0 "N" "N") ;��������
   (setq scale (getvar "USERR1"))
   (setdim 1) ;���óߴ��ע����
   (if (null (tblsearch  "dimstyle" "Normal"))
       (command "dim1" "save" "Normal")
       (command "dim1" "save" "Normal" "N")
   ) ;if end
   (setdim (/ 20.0  scale)) ;���óߴ��ע����
   (if (null (tblsearch  "dimstyle" "Dim20"))
       (command "dim1" "save" "Dim20")
       (command "dim1" "save" "Dim20" "N")
   ) ;if end
   (setdim (/ 50.0  scale)) ;���óߴ��ע����
   (if (null (tblsearch  "dimstyle" "Dim50"))
       (command "dim1" "save" "Dim50")
       (command "dim1" "save" "Dim50" "N")
   ) ;if end
  (command "dim1" "restore" "Normal" )
   (princ "\n�������Զ�������ֱ�ע��ʽ,��DDIM����鿴...")
   (resvar) ;��ԭϵͳ����
   (princ)
);defun end

