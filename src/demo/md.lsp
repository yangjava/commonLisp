;设置尺寸标注系统变量
(defun setdim(dlfac / a bl scale blx)
   (Setq bl (getvar "USERR1")
         scale bl blx (/ bl 100.0))
   (setvar "DIMDLE" 0) 
   (setvar "DIMEXO" 0) ;延伸线起点偏移原点的距离
   (setvar "DIMCLRT" 256) ;尺寸标注文本的缺省颜色为BYLAYER
   (setvar "DIMCLRD" 256) ;尺寸标注线、箭头块、引导线的缺省颜色为BYLAYER
   (setvar "DIMCLRE" 256) ;尺寸延伸线的缺省颜色为BYLAYER
   (setvar "DIMGAP" bl) ;标注文本与标注线的距离
   (setvar "DIMDEC" 0)  ;Primary Units标注文本小数点位置
   (setq a (getvar "VIEWCTR"))
   (command "insert" "ccx" a ".001" ".001" "0") ;插入块,然后删掉
   (command "erase"  "L" "")
   (setvar  "BLIPMODE" 1)
   (setvar "DIMBLK" "ccx") ;指定箭头块名称
   (setvar "DIMTSZ" 0) ;绘制指定的箭头块
   (setvar "DIMSAH" 0) ;显示DIMBLK指定的箭头块
   (setvar "DIMCEN" (* bl 2))
   (setvar "DIMDLI" 0) 
   (setvar "DIMEXE" (* bl 1.5)) ;设定标注延伸线伸出标注线的长度
   (setvar "DIMTXT" (* bl 2.5)) ;设定标注文本高度
   (setvar "DIMDLE" 0) ;设定当系统变量DIMBLK="_OBLIQUE" 时尺寸标注线伸出延伸线的距离
   (setvar "DIMSHO" 0) ;拖动相关标注时不更新
   (setvar "DIMTIH" 0) ;标注文本与标注线平行
   (setvar "DIMTOH" 0) ;标注文本与标注线平行
   (setvar "DIMTIX" 1) ;总是将标注文本置于延伸线之间
   (setvar "DIMTAD" 1) ;标注文本置于延伸线中间
   (setvar "dimtvp" 1) ;设定标注文本在标注线之上或下的位置
   (setvar "DIMSOXD" 0) ;总是绘制标注线末端箭头块
   (setvar "DIMASZ" (* bl 1)) ;指定两端箭头或图块的大小
   (setvar "DIMTOFL" 1) ;总是绘制标注线
   (setvar "DIMTMOVE" 2) ;用TEDIT命令时允许标注文本自由移动，且不为文本加引导线
   (setvar "DIMLFAC" dlfac) ;设定尺寸线放大比例
   (setvar "DIMTXSTY" "RMC") ;设定标注文本字型
   (princ)
) ;defun end

;define new dimention style,you MAY or NOT use this dimention style
(defun c:md ( / a )
   (inivar) ;初始化系统变量
   (command "style" "RMC" "simplex,hztxt" 0 0.7 0 "N" "N") ;设置字型
   (setq scale (getvar "USERR1"))
   (setdim 1) ;设置尺寸标注变量
   (if (null (tblsearch  "dimstyle" "Normal"))
       (command "dim1" "save" "Normal")
       (command "dim1" "save" "Normal" "N")
   ) ;if end
   (setdim (/ 20.0  scale)) ;设置尺寸标注变量
   (if (null (tblsearch  "dimstyle" "Dim20"))
       (command "dim1" "save" "Dim20")
       (command "dim1" "save" "Dim20" "N")
   ) ;if end
   (setdim (/ 50.0  scale)) ;设置尺寸标注变量
   (if (null (tblsearch  "dimstyle" "Dim50"))
       (command "dim1" "save" "Dim50")
       (command "dim1" "save" "Dim50" "N")
   ) ;if end
  (command "dim1" "restore" "Normal" )
   (princ "\n程序已自动设好三种标注样式,用DDIM命令查看...")
   (resvar) ;还原系统变量
   (princ)
);defun end

