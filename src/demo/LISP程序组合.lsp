;acadiso.lsp�������Զ������ļ������Ǳ��ֲ���ʹ�õ��ļ������Զ����ص��ļ���Ӧ����acad.lsp��
;�ֽ����������õĲ��ֱ༭��������ͬʱ�����򵥽��ܣ�����δ���ԣ���ͬʱҪ��fox-hu��ɾ����¥;�ĸ�������������ɾ���������������ɻ����Ǵ���������

;�������ݣ�
;�ֲ��Ŵ� ss 
;�ֲ��Ŵ� sss 
;Real Scale,���ڻ�����ͼ����ͼԪʱ,ά�ֱ�ע�ߴ粻�� rs 
;��ָ��������ظ��ƶ��󣬿���ָ������������� ds 
;��ָ�����ȴ���һ������֪�߶�ƽ�е��߶Σ�����OFFSET���Ⱦ��ߣ����� of 
;˫��ƫ�ƣ�DOUBLE OFFSET�������κ�һ�ߴ�����ѡʵ���ƫ��ʵ�� oo 
;�������иı��ı�ʵ����ַ� cht 
;�Զ���עԲ����Բ������κͷ�ն����ߵ���� aat 
;����һ�����˴���ͷ��˫�㻮���ߣ���ͷ���߿��ָ�� sj 
;����˵����������˵������������������Text Style��������ʽ����ɫ����ڵ�ǰ�� tg 
;���������һ������ƽ�壬����ָ��������λ�õ���˨�� mb 
;������ɾ��ָ�����ϵ�����ʵ�壬���۲��״̬��Ρ�ͨ�����������ָ���� dla 

;ͼ��༭ sy ls cl 
(defun c:my() (sly) (command "move" "p"""))   
(defun c:ey() (sly) (command "erase" "p""")) 
(defun c:cpy() (sly) (command "copy" "p""")) 
(defun c:cgy() (sly) (command "change" "p""""P")) 
(defun c:miy() (sly) (command "mirror" "p""")) 
(defun c:ary() (sly) (command "array" "p""")) 
(defun c:scy() (sly) (command "scale" "p""")) 
(defun c:roy() (sly) (command "rotate" "p"""))   

;ͼ�㸴�� cx 
;����ͼ�� cgc 
;������ѡ������һ���㣬���ƶ���ԭʼλ�� cf 
;���Ƶ���ǰ�� cgc 
;�ƶ��ƶ����ϵ�����ʵ�� mla 
;������ bx 
;������Ƕ� hn 
;���ش�� bbb 
;���ɴ�� bk 
;��Ļ���Ŵ󾵡� zb 
;�Զ����������ı�����ԲȦ�ͱ�ʶ ab 
;�Զ��������ֻ���ĸ�ı�����ԲȦ�ͱ�ʶ db 
;�ֶ������ı�����ԲȦ�ͱ�ʶ ddd 
;�޸���ѡԲ��ֱ�� cd 
;���˵����ͬ����x��y���꣬���Խ����ֱ��������һ���Ϊ������ pej 
;�Ƕ����г��� aar 

;�������� 
(DEFUN C:up ()(COMMAND "DIM" "UP" )) 
(DEFUN C:TTT ()(COMMAND "CIRCLE" "3P" "TAN" PAUSE "TAN" PAUSE "TAN" PAUSE)) 
(DEFUN C:TTR ()(COMMAND "CIRCLE" "TTR" )) 
(defun c:2P()(command "circle" "2p")) 
(defun c:3P()(command "ARC" "3p")) 
(DEFUN C:LL ()(COMMAND "line" "tan" PAUSE "tan" )) 
(DEFUN C:tt() (COMMAND "line" pause "tan" pause "")) 
(DEFUN C:DY ()(COMMAND "LENGTHEN" "DY" )) 
(DEFUN C:TO ()(COMMAND "LENGTHEN" "TO" )) 
(DEFUN C:DE ()(COMMAND "LENGTHEN" "DE" )) 
(DEFUN C:SP ()(COMMAND "STRETCH" "CP" )) 
(DEFUN C:ZA ()(COMMAND "zoom" "A" )) 
(DEFUN C:ZE ()(COMMAND "ZOOM" "E" )) 
(DEFUN C:ZV() (COMMAND "ZOOM" "V")) 
(DEFUN C:ZD() (COMMAND "'ZOOM" "D")) 
(DEFUN C:ZW() (COMMAND "ZOOM" "W")) 
(DEFUN C:ZP() (COMMAND "ZOOM" "P")) 
(DEFUN C:UW() (COMMAND "UCS" "W")) 
(DEFUN C:UB ()(COMMAND "UCS" "OB" )) 
(defun c:uoo() (command "UCSICON" "ON" "UCS" "o" "int")) 
(DEFUN C:UZ ()(COMMAND "UCS" "Z" )) 
(DEFUN C:U5 ()(COMMAND "UCS" "Z" "5" )) 
(DEFUN C:U10 ()(COMMAND "UCS" "Z" "10" )) 
(DEFUN C:U15 ()(COMMAND "UCS" "Z" "15" )) 
(DEFUN C:U20 ()(COMMAND "UCS" "Z" "20" )) 
(DEFUN C:U30 ()(COMMAND "UCS" "Z" "30" )) 
(DEFUN C:U45 ()(COMMAND "UCS" "Z" "45" )) 
(DEFUN C:U60 ()(COMMAND "UCS" "Z" "60" )) 
(DEFUN C:U70 ()(COMMAND "UCS" "Z" "70" )) 
(DEFUN C:U3p ()(COMMAND "UCS" "3p" )) 
(DEFUN C:HH ()(COMMAND "XLINE" "H" )) 
(DEFUN C:VV ()(COMMAND "XLINE" "V" )) 
(DEFUN C:FP ()(COMMAND "FILLET" "r" PAUSE) 
              (COMMAND "FILLET" "p" PAUSE )) 
(DEFUN C:f100 ()(COMMAND "FILLET" "R" "100" )(COMMAND "FILLET" )) 
(DEFUN C:f20 ()(COMMAND "FILLET" "R" "20" )(COMMAND "FILLET" )) 
(DEFUN C:f30 ()(COMMAND "FILLET" "R" "30" )(COMMAND "FILLET" )) 
(DEFUN C:f40 ()(COMMAND "FILLET" "R" "40" )(COMMAND "FILLET" )) 
(DEFUN C:f50 ()(COMMAND "FILLET" "R" "50" )(COMMAND "FILLET" )) 
(DEFUN C:f60 ()(COMMAND "FILLET" "R" "60" )(COMMAND "FILLET" )) 
(DEFUN C:f70 ()(COMMAND "FILLET" "R" "70" )(COMMAND "FILLET" )) 
(DEFUN C:f80 ()(COMMAND "FILLET" "R" "80" )(COMMAND "FILLET" )) 
(DEFUN C:f1 ()(COMMAND "FILLET" "R" "1" )(COMMAND "FILLET" )) 
(DEFUN C:f2 ()(COMMAND "FILLET" "R" "2" )(COMMAND "FILLET" )) 
(DEFUN C:f3 ()(COMMAND "FILLET" "R" "3" )(COMMAND "FILLET" )) 
(DEFUN C:f4 ()(COMMAND "FILLET" "R" "4" )(COMMAND "FILLET" )) 
(DEFUN C:f5 ()(COMMAND "FILLET" "R" "5" )(COMMAND "FILLET" )) 
(DEFUN C:f6 ()(COMMAND "FILLET" "R" "6" )(COMMAND "FILLET" )) 
(DEFUN C:f7 ()(COMMAND "FILLET" "R" "7" )(COMMAND "FILLET" )) 
(DEFUN C:f8 ()(COMMAND "FILLET" "R" "8" )(COMMAND "FILLET" )) 
(DEFUN C:f9 ()(COMMAND "FILLET" "R" "9" )(COMMAND "FILLET" )) 
(DEFUN C:f10 ()(COMMAND "FILLET" "R" "10" )(COMMAND "FILLET" )) 
(DEFUN C:Ch1 ()(COMMAND "CHAMFER" "D" "1" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch2 ()(COMMAND "CHAMFER" "D" "2" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch3 ()(COMMAND "CHAMFER" "D" "3" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch4 ()(COMMAND "CHAMFER" "D" "4" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch5 ()(COMMAND "CHAMFER" "D" "5" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch6 ()(COMMAND "CHAMFER" "D" "6" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch7 ()(COMMAND "CHAMFER" "D" "7" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch8 ()(COMMAND "CHAMFER" "D" "8" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch9 ()(COMMAND "CHAMFER" "D" "9" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch10 ()(COMMAND "CHAMFER" "D" "10" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch20 ()(COMMAND "CHAMFER" "D" "20" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch30 ()(COMMAND "CHAMFER" "D" "30" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch40 ()(COMMAND "CHAMFER" "D" "40" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch50 ()(COMMAND "CHAMFER" "D" "50" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch60 ()(COMMAND "CHAMFER" "D" "60" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch70 ()(COMMAND "CHAMFER" "D" "70" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch80 ()(COMMAND "CHAMFER" "D" "80" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch90 ()(COMMAND "CHAMFER" "D" "90" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch100 ()(COMMAND "CHAMFER" "D" "100" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch200 ()(COMMAND "CHAMFER" "D" "200" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch300 ()(COMMAND "CHAMFER" "D" "300" "")(COMMAND "CHAMFER" )) 
(DEFUN C:Ch500 ()(COMMAND "CHAMFER" "D" "500" "")(COMMAND "CHAMFER" )) 
(DEFUN C:m88 ()(COMMAND "insert" "m88" pause "" "" "")) 
(DEFUN C:m66 ()(COMMAND "insert" "m66" pause "" "" "")) 
(DEFUN C:m55 ()(COMMAND "insert" "m55" pause "" "" "")) 
(DEFUN C:m44 ()(COMMAND "insert" "m44" pause "" "" "")) 
(DEFUN C:L10 ()(COMMAND "insert" "L10" pause "" "" "")) 
(DEFUN C:L8 ()(COMMAND "insert" "L8" pause "" "" "")) 
(DEFUN C:L6 ()(COMMAND "insert" "L6" pause "" "" "")) 
(DEFUN C:L5 ()(COMMAND "insert" "L5" pause "" "" "")) 
(DEFUN C:L4 ()(COMMAND "insert" "L4" pause "" "" "")) 
(DEFUN C:L3 ()(COMMAND "insert" "L3" pause "" "" "")) 
(DEFUN C:DZ ()(COMMAND "insert" "DZ" pause "" "" "")) 
(DEFUN C:FS ()(COMMAND "insert" "FS" pause "" "" "")) 

(defun c:oe()(command "offset" pause pause pause "") 
             (command "erase" "p" "")) 
(defun c:od()(command "offset" pause pause pause "") 
             (command "chprop" "l" "" "c" "BYLAYER" "lt" "dashed" "")) 
(defun c:oj()(command "offset" pause pause pause "") 
             (command "chprop" "l" "" "la" "o" "")) 
(defun c:om()(command "offset" pause pause pause "") 
             (command "chprop" "l" "" "la" "mm" "")) 
(defun c:ol()(command "offset" pause pause pause "") 
             (command "laycur" "l" "")) 

(defun c:d0()(command "dimtoh" "0")(command "dim1" "up" pause "")) 
(defun c:d1()(command "dimtoh" "1")(command "dim1" "up" pause "")) 
(defun c:dn()(command "dim1" "n")) 
(defun c:dl()(command "dim1" "l")) 
(defun c:ltt()(command "_LEADER" pause pause  "A" "" "T")) 
(defun c:lM()(command "_LEADER" pause pause pause "A" "" "" "")) 
(defun c:lNN()(command "_LEADER" pause pause pause "A" "" "N")) 

(DEFUN C:qs() (Command "_ZOOM" "E" "_PURGE" "a" "" "N" "LAYER" "ON" "*" "" "_qsave"))   
(DEFUN C:lP() (Command "LAYERP")) 
(defun c:ds() (command "dimscale")) 
(DEFUN C:dr() (COMMAND  "DIMORDINATE")) 
(DEFUN C:df() (COMMAND "DIMLFAC")) 
(DEFUN C:DEC() (Command "DIMDEC")) 
(DEFUN C:DAC() (Command "DIMADEC")) 
(DEFUN C:11() (Command "osmode")) 
(DEFUN C:XO() (Command "XLINE" "O" )) 
(DEFUN C:cr() (Command "select" pause "" "copy" "p" "" "0,0" "" "rotate" "p" "")) 
(DEFUN C:cm() (Command "select" pause "copy" "p" "" "m" pause)) 
(DEFUN C:Xa() (Command "XLINE" "a")) 
(DEFUN C:Xb() (Command "XLINE" "b")) 
(DEFUN C:UM() (Command "UNDO" "M" )) 
(DEFUN C:UD() (Command "UNDO" "B" )) 
(DEFUN C:mn() (Command "move" "l" "" "0,8" "")) 
(DEFUN C:bp() (COMMAND "-boundary" pause "" ) 
              (COMMAND "chprop" "l" "" "la" pause "" 
                       "erase"  "c"  pause pause "R"  "L" )) 
(DEFUN C:EF() (SSGET) (Command "ERASE" "P" "R" "L" "")) 
(DEFUN C:ds1() (COMMAND "dimstyle" "r" "1")) 
(DEFUN C:ds2() (COMMAND "dimstyle" "r" "2")) 
(DEFUN C:ds3() (COMMAND "dimstyle" "r" "3")) 
(DEFUN C:ds4() (COMMAND "dimstyle" "r" "4"))    
(DEFUN C:ly() (COMMAND "-LAYER" "ON" "*" "")) 
(DEFUN C:ln()  (COMMAND "-LAYER" "S" pause "OFF" "*" "" "")) 
(DEFUN C:lo() (COMMAND "-LAYER" "OFF" )) 
(defun c:gm()(ssget)(command "chprop" "p" "" "la" "mm" "")) 
(defun c:go()(ssget)(command "chprop" "p" "" "la" "o" "")) 
(defun c:c3()(ssget)(command "chprop" "p" "" "c" "3" "")) 
(defun c:c4()(ssget)(command "chprop" "p" "" "c" "4" "")) 
(defun c:c5()(ssget)(command "chprop" "p" "" "c" "5" "")) 
(defun c:c6()(ssget)(command "chprop" "p" "" "c" "6" "")) 
(defun c:c7()(ssget)(command "chprop" "p" "" "c" "7" "")) 
(defun c:c8()(ssget)(command "chprop" "p" "" "c" "8" "")) 
(defun c:c250()(ssget)(command "chprop" "p" "" "c" "250" "")) 
(defun c:po()(command "pedit" pause "" "j" "c" pause pause "" "") 
             (command "offset" pause pause pause "")) 
(defun c:pr()(command "pedit" pause "" "j" "c" pause pause "" "") 
             (command "fillet" "r" pause) 
             (command "fillet" "p" pause )) 
(defun c:pj ()(command "pedit" pause "y" "j" "all" "" "") 
(princ) 
) 
(defun c:ld()(command "linetype" "l" "center,divide,dashed" "" "") 
             (command "ltscale" pause "")) ;�i?/font>@һ���Ե��������ߣ����ߣ�˫�㻮�� 
(defun c:sk()(command "skpoly" "1")(command "sketch" "0.01")) 
(defun c:aa()(command "area" "e")) 
(defun C:tp() (command "dimtol" "1" "dimtp")) 
(defun C:tm() (command "dimtol" "1" "dimtm")) 
(defun C:tf() (command "dimtol" "0")) 
(defun C:tn() (command "dimtol" "1" "DIMTFAC" "0.7"))


;==========�Լ����lsp=============================================
(DEFUN C:ss( / ENT P PC K R N EP)
  (PRINC "\n �ֲ��Ŵ� \(2005.8.8������\) ")
  (PRINC "\n ��ѡ��Ŵ��Բ������:")
  (SETQ ENT (CAR (ENTSEL))                       ;��÷Ŵ�����
        PC (CDR (ASSOC 10 (ENTGET ENT)))         ;Բ������
        P (GETPOINT "\n ��ѡ������")           ;�����
        K (GETREAL "\n ���������ű���:")         ;�Ŵ����
        R (CDR (ASSOC 40 (ENTGET ENT)))          ;�뾶
  )
  (SETVAR "OSMODE" 0)(SETVAR "ORTHOMODE" 0)
  (COMMAND "_COPY" "CP")
  (SETQ N 0)
  (REPEAT 180
    (COMMAND (POLAR PC (/ (* 2 N PI) 180) R ))
    (SETQ N (1+ N))
  )
  (COMMAND "" "" PC P)
  (SETQ N 0)
  (SETVAR "PICKBOX" 5)
  (COMMAND "ZOOM" "C" P (* 3 R))
  (COMMAND "CIRCLE" P R)
  (SETQ EP (ENTLAST))
  (COMMAND "TRIM" EP "")
  (REPEAT 180
     (COMMAND "F" (POLAR P (/ (* 2 N PI) 180) (* R 1.01)))
     (SETQ N (1+ N))
     (COMMAND (POLAR P (/ (* 2 N PI) 180) (* R 1.01)) "")
  )
  (COMMAND "" "ZOOM" "P")
  (COMMAND "SCALE" "C" (LIST (- (CAR P ) R) (- (CADR P ) R) )
              (LIST (+ (CAR P) R) (+ (CADR P) R)) "" P K)
  (COMMAND "ERASE" EP "")
  (COMMAND "REDRAW")
  (SETVAR "OSMODE" 37)(SETVAR "ORTHOMODE" 1)
  (PRINC "\nOK!")(PRINC)
  (PRINC)
 )


(DEFUN C:sSs( / ENT P PC K R N EP)
  (PRINC "\n �ֲ��Ŵ� \(2005.8.8������\) ")
  (PRINC "\n ��ѡ��Ŵ��Բ������:")
  (SETQ ENT (CAR (ENTSEL))                       ;��÷Ŵ�����
        PC (CDR (ASSOC 10 (ENTGET ENT)))         ;Բ������
        P (GETPOINT "\n ��ѡ������")           ;�����
        K (GETREAL "\n ���������ű���:")         ;�Ŵ����
        R (CDR (ASSOC 40 (ENTGET ENT)))          ;�뾶
  )
  (SETVAR "OSMODE" 0)(SETVAR "ORTHOMODE" 0)
  (COMMAND "copy" "cp")
  (SETQ N 0)
  (REPEAT 180
    (COMMAND (POLAR PC (/ (* 2 N PI) 180) R ))
    (SETQ N (1+ N))
  )
  (COMMAND "" "" PC P)
  (SETQ N 0)
  (SETVAR "PICKBOX" 5)
  (COMMAND "ZOOM" "C" P (* 3 R))
  (COMMAND "CIRCLE" P R)
  (SETQ EP (ENTLAST))
  (COMMAND "TRIM" EP "")
  (REPEAT 180
     (COMMAND "F" (POLAR P (/ (* 2 N PI) 180) (* R 1.01)))
     (SETQ N (1+ N))
     (COMMAND (POLAR P (/ (* 2 N PI) 180) (* R 1.01)) "")
  )
  (COMMAND "" "ZOOM" "P")
  (COMMAND "SCALE" "C" (LIST (- (CAR P ) R) (- (CADR P ) R) )
              (LIST (+ (CAR P) R) (+ (CADR P) R)) "" P K)
  (COMMAND "ERASE" EP "")
  (COMMAND "REDRAW")
  (SETVAR "OSMODE" 37)(SETVAR "ORTHOMODE" 1)

  (PRINC "\nOK!")(PRINC)
  (PRINC)
 )
;=====================================================================================
(defun c:RS ( )               ;Real Scale,���ڻ�����ͼ����ͼԪʱ,ά�ֱ�ע�ߴ粻��
  (princ "ͼԪ����,�ߴ粻��\(2005.8.8������\)")
  (PROMPT "\nָʾ��������:")
  (SETVAR "OSMODE" 0)(SETVAR "ORTHOMODE" 0)
  (setq scale (ssget ))
  (SETQ ENTGRP scale)
  (SETQ COUNT 0)                                         
  (REPEAT (SSLENGTH ENTGRP)
    (SETQ ENTNAME (SSNAME ENTGRP COUNT))
    (SETQ ENT (ENTGET ENTNAME))
    (IF (AND (= (CDR (ASSOC 0 ENT)) "DIMENSION")        
             (/= (cdr (ASSOC 1 ENT)) ""))     
       (PROGN (PRINC "\n�����в������޸Ĺ��ĳߴ�!") (EXIT)))
       (IF (and(= (CDR (ASSOC 0 ENT)) "DIMENSION")                ;AlignedDimension
            (= (cdr (nth 19 ent)) "AcDbAlignedDimension"))       
          (PROGN
              (setq P1 (assoc 13 ENT)) 
              (setq P2 (assoc 14 ENT))
              (setq P1x (nth 1 P1))  
              (setq P1y (nth 2 P1))                   
              (setq P2x (nth 1 P2)) 
              (setq P2y (nth 2 P2)) 
              (setq DX (ABS(- P1x P2x)))           
              (setq DY (ABS(- P1y P2y)))            
              (setq SS (assoc 10 ENT)) 
              (setq TT (assoc 11 ENT)) 
              (setq SSx (nth 1 SS))  
              (setq TTx (nth 1 TT)) 
              (setq SSy (nth 2 SS))  
              (setq TTy (nth 2 TT))    
              (IF (= SSx TTx)                    
                 (setq V Dy))
              (IF (and(/= SSx TTx)(= SSy TTy)) 
                 (setq V Dx))                                      ;ȡ����ֵV1
              (IF (and(/= SSx TTx)(/= SSy TTy)) 
                 (setq V (distance (cdr p1) (cdr p2))))              
              (SETQ W (rtos V 2 2))
              (SETQ ENT (SUBST (CONS 1 W) (ASSOC 1 ENT) ENT))       ;MODIFY
              (ENTMOD ENT)                                          ;UPDATE
          )
      )
    (IF (and(= (CDR (ASSOC 0 ENT)) "DIMENSION")                    ;RadialDimension
            (= (cdr (nth 19 ent)) "AcDbRadialDimension"))
          (PROGN
              (setq P1 (assoc 10 ENT)) 
              (setq P2 (assoc 15 ENT))
              (setq V (distance (cdr p1) (cdr p2)))                ;ȡ����ֵV1            
              (SETQ W (strcat "R" (rtos V 2 2)))
              (SETQ ENT (SUBST (CONS 1 W) (ASSOC 1 ENT) ENT))       ;MODIFY
              (ENTMOD ENT)                                          ;UPDATE
          )
      )
  (SETQ COUNT (1+ COUNT))
  )
  (setq base (getpoint "\n /Base point:"))(princ "Done")
  (SETQ factor (getint "\n /Scale factor:"))
  (command "_scale" scale "" base factor)
  (SETQ OLDLAYER (GETVAR "CLAYER"))
  (COMMAND "-LAYER" "S" "T" "") 
  (setq txtSTRING (strcat "SCALE " (RTOS factor 2 0) ":1"))                
  (COMMAND "_.TEXT" "J" "C" base (* 2.5 (GETVAR "DIMSCALE")) "" (strcase txtSTRING ))
  (COMMAND "-LAYER" "S" OLDLAYER "") 
  (SETVAR "OSMODE"37)(SETVAR "ORTHOMODE" 1) 
  (PRINC "\n OK! ")
  (PRINC)
)

;=================================================================================
;��ָ��������ظ��ƶ��󣬿���ָ���������������

(defun C:DC ()
  (princ "��ָ��������ظ��ƶ��󣬿���ָ���������������(2005.8.8������\)")
  (setq A nil)
  (setq OM (getvar "OSMODE"))
  (setvar "OSMODE" 3007)
  (setq PNT1 (getpoint "\nʰȡ��һ�㣺  "))
  (setq PNT2 (getpoint "\nʰȡ�ڶ��㣺 " PNT1))(terpri)
  (initget 1 "M E N")
  (prompt "\nѡ������֮һ�� ")
  (setq CTYPE
  (getkword "\n(M)�����  (E)��ȷ���  (N)������ "))
  (if (= CTYPE "M")
  (setq SP (getdist "\n�������ࣺ ")))
  (if (= CTYPE "E")
  (setq SP (getdist "\n��ȷ�����ࣺ ")))
  (if (= CTYPE "N")
  (setq SP (getreal "\n���������� ")))
  (setq DIST (distance PNT1 PNT2))
  (setq ANG (angle PNT1 PNT2))
  (setq TEMP1 (/ DIST SP))
  (setq TEMP2 (fix (/ DIST SP)))
  (setq INC1 SP)
  (setq INC2 (/ DIST (+ 1 (fix (/ DIST SP)))))
  (setq INC3 (/ DIST (- SP 1)))
  (if (= TEMP1 TEMP2) (setq INC INC1) (setq INC INC2))
  (if (= CTYPE "E") (setq INC INC1) (setq INC INC))
  (if (= CTYPE "N") (setq INC INC3) (setq INC INC))
  (setq TMS (FIX (+ 0.00001 (/ DIST INC))))
  (setvar "OSMODE" 3007)
  (setq A (ssget))
  (setq INCR 0)
  (repeat TMS
    (setq INCR (+ INCR INC))
    (setq NEWPT (polar PNT1 ANG INCR))
    (command "copy" A "" PNT1 NEWPT)
  )
  (setvar "OSMODE" 3007)
  (setq A nil)
);end cm.lsp


;======================================================================================
;��ָ�����ȴ���һ������֪�߶�ƽ�е��߶Σ�����OFFSET���Ⱦ��ߣ����

(defun c:OF (/ d l pick ent type1 type ent1 pt2 newent newp1 newp2 p1 p2 f f1 f2 f3 f4)
  (princ "��ָ�����ȴ���һ������֪�߶�ƽ�е��߶Ρ�(2005.8.8������\)")
        (setvar "cmdecho" 0)
	(setq d (getreal "\n����ƫ�ƾ��룺 "))
	(setq l (getreal "\n�����µ�ʵ�峤�ȣ� "))
	(setq pick (entsel "\nʰȡҪƫ�ƵĶ���  "))
	(setq ent (car pick))
	(setq type1 (entget (car pick)))
	(setq ent1 (cdr(assoc 0 type1)))
        	(if (= ent1 "LINE")
	  (progn
	     (setq pt2 (getpoint "\nƫ�Ʒ��� "))
	     (command "offset" d ent pt2 "")
	     (setq newent (entlast))
	     (setq type (entget newent))
		(setq p1 (cdr(assoc 10 type)))
		(setq p2 (cdr(assoc 11 type)))
		(setq f3 (assoc 8 type))
       		(setq ang (angle p1 p2))
		(setq ang2 (angle p2 p1))
		(setq dis (/ (distance p1 p2) 2))
		(setq mid (polar p1 ang dis))
		(setq newp1 (polar mid ang2 (/ l 2)))
	        (setq newp2 (polar mid ang(/ l 2)))
		(setq f (cons 10 newp1))
	        (setq f1 (cons 11 newp2))
		(setq f2 (cons 0 "LINE"))
		(setq final (list f2 f3 f f1))
	        (entmake final)
		(command "erase" newent "")
	  );close progn
	(Prompt "\n��ʵ�岻��һ���ߣ�")
	);end if
	(command "redraw")
	(setvar "cmdecho" 1)
	(princ)
	(princ)
)
(prompt "\n����OF��������: ")




;=================================================================
;˫��ƫ�ƣ�DOUBLE OFFSET�������κ�һ�ߴ�����ѡʵ���ƫ��ʵ�塣

(defun c:OO (/ a b b1 p1 d pckbox)
(PRINC "\ ˫��ƫ�� \(2005.8.8������\) ")
(setq d 20 p1 nil pee (/ pi 2)e 3 pckbox (getvar "pickbox")line " ")
(if(= #parof nil)(setq #parof(/(getvar "viewsize")10)))
(princ "\n��ǰƫ���� <")(princ #parof)
(setq answer(getstring ">: "))
(if (/= (atof answer) 0.0)(setq #parof(atof answer)))
	(princ line)
	(setvar "pickbox" pckbox)
	(setq ent(entsel "\nʰȡ�ߣ� "))
	(setvar "pickbox" 2)
	(setq point(cadr ent))
	(princ line)
        (setq side(getpoint "\nʰȡҪƫ�Ƶ�һ���� "))
	(setq dist(distance side point))(setq ang(angle side point))
	(if(or(or(< ang 0.78)(> ang 5.5))(and(> ang 2.35)(< ang 3.92)))
		(setq ang(- 0 ang))(setq ang(- pi ang))
	);end if
	(setq other(polar point ang dist))
        (command "offset" #parof ent side ent other "")
        ;(entdel(car ent))

(setvar "pickbox" pckbox)
(princ)
(princ)
)
(Prompt "\n����OO�������")

; =========================================================================
;������ʾʹ��ENTMOD�����ı�ʵ�����ԡ�
;���롰CHT���ı��ı�ʵ����ַ�����
 
(defun c:cht ( / ent string )
     (PRINC "\ �������иı��ı�ʵ����ַ�����(2005.8.8������\) ")
	(setq ent (entget (car (entsel "\nѡ��Ҫ�༭���ı��� "))))
	(if (= (cdr (assoc 0 ent)) "TEXT")
		(progn
			(setq string (getstring T "\n�������ı��� "))
			(setq ent (subst (cons 1 string) (assoc 1 ent) ent))
			(entmod ent)
		)
	)
	(princ)
)

(prompt "\���롰CHT���ı��ı�ʵ����ַ����� ")



; =====================================================================================
;�Զ���עԲ����Բ������κͷ�ն����ߵ������

(defun C:aat (/ e th val unit fmt pt )
  (PRINC "\n �Զ���עԲ����Բ������κͷ�ն����ߵ���� \(2005.8.5������\) ")
  (setvar "cmdecho" 0)
  (setq e (entsel "\n��ѡ��һ��ʵ��..."))
  (command "area" "e" e)(setq th (* 1 (getvar "dimscale")))
  (setq VAL (getvar "area" ))                               
  (setq unit (getvar "lunits"))
  (setq txt_style (getvar "textstyle"))
  (command "style" "standard" "" "" "" "" "" "" "")
    (if (or(= unit 4)(= unit 5))
       (progn
         (setq val (/ val 144))
         (setq fmt (strcat (rtos val 2 ) " sq.in.")) 
         (setq PT (getvar "viewctr"))
         (command "text" PT TH "0" Fmt)
         (princ "�����ı���")
         (command "move" "SI" "L" PT Pause)
       );ends progn
       (progn
         (setq fmt (rtos val))
         (setq PT (getvar "viewctr"))
         (command "text" PT TH "0" Fmt)
         (princ " �����ı���")
         (command "move" "SI" "L" PT pause)
       ); Ends progn 
   ); Ends if
   (setvar "textstyle" txt_style)
   (setvar "cmdecho" 1)
   (princ)
); ends atxt.lsp
(prompt "\n����AAT�������")


;===============================================================================
; ����һ�����˴���ͷ��˫�㻮���ߣ���ͷ���߿��ָ����

(defun c:sj ()
  (PRINC "\n ���˴���ͷ \(2005.8.5������\) ")
  (setvar "cmdecho" 0)
  (if (not asize) (setq asize 2.0))      ;��ʼ����ͷ�ߴ�
  (if (not PThk)  (setq PThk 0.1))      ;�����߿��
  (defun GETR (val msg / tm)
    (setq tm (getreal (strcat msg " <" (rtos val 2 4) ">: ")))
    (cond ((= (type tm) 'REAL) (eval tm))
          ((= tm nil) (eval val))
          (t (princ "\007 *����* ����һ��ʵ��") (eval val)) ) )
  (defun loop ()
    (cond ((setq p2 (getpoint p1 "���㣺 ")) (command p2) 
                                     (setq p0 p1) (setq p1 p2) (loop))
          ( t (command "u" (polar p1 (angle p1 p0) asize)
                       "w" (/ asize 3) 0.0 p1 ""))))
  (setq asize (getr asize "��ͷ��С"))
  (setq PThk  (getr PThk " �����߿��"))
  (setq p1 (getpoint " �ӵ㣺 "))
  (command "pline" p1 "w" 0.0 0.0)
  (setq p2 (getpoint p1 " ���㣺 "))
  (command "w" 0.0 (/ asize 3) (polar p1 (angle p1 p2) asize) 
           "w" PThk PThk p2)
  (setq p1 p2)
  (loop)
  (eval "very good !!!")
)
(prompt "\n����SJ������� ")

;================================================================================================
;����һ�����б�ǩ����ע�߲������ı�ʶ�ȡ�
;��ָ����ע�߼�ˡ��սǵ㡢��ǩλ�á���ǩ��������Ҫ�Ķ��������Լ��Ƿ�Ը�⽫�ı����ڱ�ǩ���Ϸ����·���
;���û�������ǩ��������������ɱ�׼�ı�ע��
;�����Զ�ʶ������һ�߻��Ƶı�ǩ����Ӧ�����������ı���
;����ʹ��TEXTSIZEϵͳ����ȷ���ı��ߣ����ȷ�����ʵ�������ϵͳ������

(defun tag_error (msg)
      (if (/= msg "Function cancelled")
            (princ (strcat "\n����  " msg))
      )
      (setvar "orthomode" tag_ortho)
      (setvar "osmode" tag_snap)
      (setq *error* old_error)
      (princ)
)

(defun c:tg()
      (PRINC "\n ����˵����������˵������������������Text Style��������ʽ����ɫ����ڵ�ǰ�㡣 \(2005.8.5������\) ")
      (setq old_error *error*)
      (setq *error* tag_error)
      (setq l_num 0)
      (setq txtw 1.25)
      (setq tag_desc ())
      (setq hgt (getvar "textsize"))
      (setq tag_ortho (getvar "orthomode"))
      (setq tag_snap (getvar "osmode"))
      (setvar "orthomode" 0)
      (setvar "osmode" 0)
      (setq tag_tip (getpoint "\nʰȡ����˵���߶˵�..."))
      (setq tag_elbow (getpoint tag_tip "\n�յ�..."))
      (setq tag_arrow (polar tag_tip (angle tag_tip tag_elbow) (* 1.25 hgt)))
      (setvar "orthomode" 1)
      (setq tag_box (getpoint tag_elbow "\n˵���˵�..."))
      (setq tag_num (getstring  "\n˵���� "))
      (while (/= (setq tag_line (getstring 1 "������ ")) "")
            (setq l_num (1+ l_num))
            (setq tag_desc (append tag_desc (list tag_line)))
      )
      (if (null txt_loc) (setq txt_loc "B"))
      (setq loc (getstring (strcat "�������� \050A\051���ڻ�\050B\051����˵���� <" txt_loc "> ?  ")))
      (if (/= loc "")
            (if (member loc (list "A" "a" "ABOVE" "above" "Above")) (setq txt_loc "A") (setq txt_loc "B"))
      )
      (entmake (list (cons 0 "POLYLINE")
            (cons 8 (getvar "clayer"))
            (cons 66 1)
            (cons 10 (list 0.0 0.0 0.0))
            (cons 40 0.0)
            (cons 41 0.0)
            (cons 62 256)
            (cons 6 "BYLAYER")
      ))
      (entmake (list (cons 0 "VERTEX")
            (cons 10 tag_tip)
            (cons 41 (* 1.00 (getvar "textsize")))
      ))
      (entmake (list (cons 0 "VERTEX")
            (cons 10 tag_arrow)
      ))
      (entmake (list (cons 0 "VERTEX")
            (cons 10 tag_elbow)
      ))
      (entmake (list (cons 0 "VERTEX")
            (cons 10 tag_box)
      ))
      (cond
            ((and (> (car tag_box) (car tag_elbow)) (/= tag_num""))
                  (progn
                        (setq tag_orient "right")
                        (entmake (list (cons 0 "VERTEX")
                              (cons 10 
                                    (list 
                                          (+ (car tag_box) (* 0.75 hgt))
                                          (+ (cadr tag_box) (* 0.90 hgt))
                                    0.0)
                              )
                        ))
                        (entmake (list (cons 0 "VERTEX")
                              (cons 10
                                    (list 
                                          (+ (car tag_box) (* (strlen tag_num) txtw hgt) (* 0.75 hgt))
                                          (+ (cadr tag_box) (* 0.90 hgt))
                                    0.0)
                              )
                        ))
                        (entmake (list (cons 0 "VERTEX")
                              (cons 10
                                    (list 
                                          (+ (car tag_box) (* (strlen tag_num) txtw hgt) (* 1.5 hgt))
                                          (cadr tag_box)
                                    0.0)
                              )
                        ))
                        (entmake (list (cons 0 "VERTEX")
                              (cons 10 
                                    (list 
                                          (+ (car tag_box) (* (strlen tag_num) txtw hgt) (* 0.75 hgt))
                                          (- (cadr tag_box) (* 0.90 hgt))
                                    0.0)
                              )
                        ))
                        (entmake (list (cons 0 "VERTEX")
                              (cons 10
                                    (list 
                                          (+ (car tag_box) (* 0.75 hgt))
                                          (- (cadr tag_box) (* 0.90 hgt))
                                    0.0)
                              )
                        ))
                        (entmake (list (cons 0 "VERTEX")
                              (cons 10 tag_box)
                        ))
                        (setq mid 
                              (list
                                    (/ (+ (car tag_box) (+ (car tag_box) (* (strlen tag_num) txtw hgt) (* 1.5 hgt))) 2)
                                    (cadr tag_box)
                              0.0)
                        )
                  )
            )
            ((and (< (car tag_box) (car tag_elbow)) (/= tag_num""))
                  (progn
                        (setq tag_orient "left")
                        (entmake (list (cons 0 "VERTEX")
                              (cons 10
                                    (list 
                                          (- (car tag_box) (* 0.75 hgt))
                                          (+ (cadr tag_box) (* 0.90 hgt))
                                    0.0)
                              )
                        ))
                        (entmake (list (cons 0 "VERTEX")
                              (cons 10
                                    (list 
                                          (- (car tag_box) (* (strlen tag_num) txtw hgt) (* 0.75 hgt))
                                          (+ (cadr tag_box) (* 0.90 hgt))
                                    0.0)
                              )
                        ))
                        (entmake (list (cons 0 "VERTEX")
                              (cons 10
                                    (list 
                                          (- (car tag_box) (* (strlen tag_num) txtw hgt) (* 1.5 hgt))
                                          (cadr tag_box)
                                    0.0)
                              )
                        ))
                        (entmake (list (cons 0 "VERTEX")
                              (cons 10
                                    (list 
                                          (- (car tag_box) (* (strlen tag_num) txtw hgt) (* 0.75 hgt))
                                          (- (cadr tag_box) (* 0.90 hgt))
                                    0.0)
                              )
                        ))
                        (entmake (list (cons 0 "VERTEX")
                              (cons 10 
                                    (list 
                                          (- (car tag_box) (* 0.75 hgt))
                                          (- (cadr tag_box) (* 0.90 hgt))
                                    0.0)
                              )
                        ))
                        (entmake (list (cons 0 "VERTEX")
                              (cons 10 tag_box)
                        ))
                        (setq mid 
                              (list
                                    (/ (+ (car tag_box) (- (car tag_box) (* (strlen tag_num) txtw hgt) (* 1.5 hgt))) 2)
                                    (cadr tag_box)
                              0.0)
                        )
                  )
            )
      )
      (entmake (list (cons 0 "SEQEND")))
      (cond
            ((and (= txt_loc "A") (/= tag_num ""))
                  (progn
                        (if (= tag_orient "right")
                              (setq desc_base
                                    (list 
                                          (+ (car tag_box) (* 0.75 hgt))
                                          (+ (cadr tag_box) (* 1.5 hgt (length tag_desc)))
                                    0.0)
                              )
                              (setq desc_base
                                    (list 
                                          (- (car tag_box) (* 0.75 hgt))
                                          (+ (cadr tag_box) (* 1.5 hgt (length tag_desc)))
                                    0.0)
                              )
                        )
                  )
            )
            ((and (= txt_loc "B") (/= tag_num ""))
                  (progn
                        (if (= tag_orient "right")
                              (setq desc_base
                                    (list 
                                          (+ (car tag_box) (* 0.75 hgt))
                                          (- (cadr tag_box) (+ hgt (* 1.5 hgt)))
                                    0.0)
                              )
                              (setq desc_base
                                    (list 
                                          (- (car tag_box) (* 0.75 hgt))
                                          (- (cadr tag_box) (+ hgt (* 1.5 hgt)))
                                    0.0)
                              )
                        )
                  )
            )
            ((and (= txt_loc "A") (= tag_num ""))
                  (progn
                        (if (> (car tag_box) (car tag_elbow))
                              (setq desc_base
                                    (list 
                                          (+ (car tag_box) (* 0.75 hgt))
                                          (+ (cadr tag_box) (* 1.5 hgt (length tag_desc)))
                                    0.0)
                              )
                              (setq desc_base
                                    (list 
                                          (- (car tag_box) (* 0.75 hgt))
                                          (- (+ (cadr tag_box) (* 1.5 hgt (length tag_desc))) (* 0.75 hgt))
                                    0.0)
                              )
                        )
                  )
            )
            ((and (= txt_loc "B") (= tag_num ""))
                  (progn
                        (if (> (car tag_box) (car tag_elbow))
                              (setq desc_base
                                    (list 
                                          (+ (car tag_box) (* 0.75 hgt))
                                          (cadr tag_box)
                                    0.0)
                              )
                              (setq desc_base
                                    (list 
                                          (- (car tag_box) (* 0.75 hgt))
                                          (cadr tag_box)
                                    0.0)
                              )
                        )
                  )
            )
      )
      (if (/= tag_num "")
            (entmake (list (cons 0 "TEXT")
                  (cons 8 (getvar "clayer"))
                  (cons 10 mid)
                  (cons 40 hgt)
                  (cons 1 tag_num)
                  (cons 72 1)
                  (cons 73 2)
                  (cons 11 mid)
                  (cons 62 256)
                  (cons 6 "BYLAYER")
            ))
      )
      (setq c 0)
      (cond
            ((and (= tag_orient "right") (/= tag_num ""))
                  (foreach txt_line tag_desc
                        (entmake (list (cons 0 "TEXT")
                              (cons 8 (getvar "clayer"))
                              (cons 10
                                    (list 
                                          (car desc_base)
                                          (- (cadr desc_base) (* 1.5 hgt c))
                                    0.0)
                              )
                              (cons 40 hgt)
                              (cons 1 txt_line)
                              (cons 62 256)
                              (cons 6 "BYLAYER")
                        ))
                        (setq c (1+ c))
                  )
            )      
            ((and (= tag_orient "left") (/= tag_num ""))
                  (foreach txt_line tag_desc
                        (entmake (list (cons 0 "TEXT")
                              (cons 8 (getvar "clayer"))
                              (cons 10
                                    (list 
                                          (car desc_base)
                                          (- (cadr desc_base) (* 1.5 hgt c))
                                    0.0)
                              )
                              (cons 40 hgt)
                              (cons 1 txt_line)
                              (cons 72 2)
                              (cons 73 0)
                              (cons 11
                                    (list 
                                          (car desc_base)
                                          (- (cadr desc_base) (* 1.5 hgt c))
                                    0.0)
                              )
                              (cons 62 256)
                              (cons 6 "BYLAYER")
                        ))
                        (setq c (1+ c))
                  )
            )
            ((and (> (car tag_box) (car tag_elbow)) (= tag_num ""))
                  (foreach txt_line tag_desc
                        (entmake (list (cons 0 "TEXT")
                              (cons 8 (getvar "clayer"))
                              (cons 10
                                    (list 
                                          (car desc_base)
                                          (- (cadr desc_base) (* 1.5 hgt c))
                                    0.0)
                              )
                              (cons 40 hgt)
                              (cons 1 txt_line)
                              (cons 73 2)
                              (cons 11
                                    (list 
                                          (car desc_base)
                                          (- (cadr desc_base) (* 1.5 hgt c))
                                    0.0)
                              )
                              (cons 62 256)
                              (cons 6 "BYLAYER")
                        ))
                        (setq c (1+ c))
                  )
            )      
            ((and (< (car tag_box) (car tag_elbow)) (= tag_num ""))
                  (foreach txt_line tag_desc
                        (entmake (list (cons 0 "TEXT")
                              (cons 8 (getvar "clayer"))
                              (cons 10
                                    (list 
                                          (car desc_base)
                                          (- (cadr desc_base) (* 1.5 hgt c))
                                    0.0)
                              )
                              (cons 40 hgt)
                              (cons 1 txt_line)
                              (cons 72 2)
                              (cons 73 2)
                              (cons 11
                                    (list 
                                          (car desc_base)
                                          (- (cadr desc_base) (* 1.5 hgt c))
                                    0.0)
                              )
                              (cons 62 256)
                              (cons 6 "BYLAYER")
                        ))
                        (setq c (1+ c))
                  )
            )
      )      
      (setvar "orthomode" tag_ortho)
      (setvar "osmode" tag_snap)
)

; ====================================================================================
; ƽ����������
;���������һ������ƽ�壬����ָ��������λ�õ���˨�ס�

(defun c:mb ()
(setvar "cmdecho" 0)
(setq pt1 (getpoint "\nѡ��ƽ�����½ǣ� "))
(setq pt3 (getcorner pt1 "\nѡ��ƽ�����Ͻǣ� "))
(setq pt2 (list (car pt1)(cadr pt3)))
(setq pt4 (list (car pt3)(cadr pt1)))
(command "pline" pt1 pt2 pt3 pt4 "c")   
(setq cp (getpoint "\nѡ�����½���˨��λ�ã� "))
(setq dx (- (car cp)(car pt1)))
(setq dy (- (cadr cp)(cadr pt1)))
    (setq num 1)
    (while (= num 1)
(setq num (getint "\n������˨����ƽ��ˮƽ�������Ŀ�� "))
(if (= num 1)(princ "��Ч����˨����Ŀ"))
    )
    (setq num1 1)
    (while (= num1 1)
(setq num1 (getint "\n������˨����ƽ�崹ֱ�������Ŀ�� "))
        (if (= num1 nil)(setq num1 num))
        (if (= num1 1)(princ "��Ч����˨����Ŀ"))
    )
(setq dist1 (- (distance pt1 pt4) (* 2 dx)))
(setq dist2  (- (distance pt1 pt2)(* 2 dy))) 
(setq xspace (/ dist1 (- num 1)))
(setq yspace (/ dist2 (- num1 1)))
(setq rad (getdist cp "\n�����ѡ����˨�װ뾶�� "))
(command "circle" cp rad)
(setq ent (entlast))
(command "array" ent "" "r" num1 num yspace xspace)
(setvar "cmdecho" 1)
(princ)
(princ)
)
(prompt "����MB������� ")

;==========================================================                    
;dellayer.lsp
;������ɾ��ָ�����ϵ�����ʵ�壬���۲��״̬��Ρ�ͨ�����������ָ���㡣

(defun dellerr (s)                    ; ���������ڣ���CTRL-C����������������ʱ...
  (if (/= s "Function cancelled")
    (princ (strcat "\n���� " s))
  )
  (setq sset_1 nil)                   ; ����ѡ��
  (setq sset_2 nil)
  (setvar "CMDECHO" ocmd)             ; �ָ������ģʽ
  (setq *error* olderr)               ; �ָ��ɵ�*����*���
  (princ)
)

(defun c:dla ( / sset_1 sset_2 prg num count ex)
   (setq olderr *error*
        *error* dellerr)
   (setq ocmd (getvar "CMDECHO"))
   (setvar "CMDECHO" 0)
   (setq lname (strcase (getstring "\nҪɾ���Ĳ㣺 ")))
   ;ȡ��������ʵ��
   (setq sset_1 (ssget "X" (list (cons 8 lname))))
   (if sset_1
      (progn
         (initget "Yes No")
         (setq prg (getkword "\n׼���رղ��� <Y>/N:"))
         (if (= prg nil) (setq prg "no"))
         (setq num (sslength sset_1))
         (setq count 0)                      ;ɾ��ʵ��
         (repeat (sslength sset_1)
            (entdel (ssname sset_1 count))
            (setq count (1+ count))
         )
         ;Check that everything is gone
         (if
            (ssget "X" (list (cons 8 lname)))
            ;And if anything is left
            (progn
              (setq tm (getvar "tilemode"))
                (if (= 1 tm)
                  (setvar "tilemode" 0)
                )
                ;Go to paperspace
                (if (/= 1 (getvar "cvport"))
                 (progn
                  (princ "\n�л���ͼֽ�ռ䡣")
                  (command "_.pspace")
                 )
                )
                ;And try again
                (setq sset_2 (ssget "X" (list (cons 8 lname))))
                (setq count 0)
                 (repeat (sslength sset_2)
                   (entdel (ssname sset_2 count))
                   (setq count (1+ count))
                 )
              (setvar "tilemode" tm)
            )
         )
         (if (= prg "Yes")
           (progn
             ;׼�������
             ;�رա�����ͽ�����
             (setq ex (getvar "expert"))
             (setvar "expert" 5)
             (command "_.layer" "_off" lname "_thaw" lname "")
               (if (= 0 (getvar "tilemode"))
                 (command "_.vplayer" "_vpvisdflt" lname "_thaw" "_reset"
                 lname "_all" "")
               )
             (princ "\n")
             (princ lname)
             (princ " ���ϵ� ")
             (princ num)            
             (princ " ��ʵ����ɾ����")
             (princ "\n�� ") 
             (princ lname)             
             (princ " �ѽⶳ���رա�")
             (setvar "expert" ex)
           )
           (progn
           (princ "\n")
           (princ lname)
           (princ "���ϵ� ")
           (princ num)
           (princ " ��ʵ����ɾ����")
           )
         )
      )
      (princ "��Ϊ�ջ�û�кϷ��Ĳ�����")
   )
   (setq sset_1 nil)                   ; ����ѡ��
   (setq sset_2 nil)
   (setvar "CMDECHO" ocmd)             ; �ָ������ģʽ
   (setq *error* olderr)               ; �ָ��ɵ�*����*���
   (princ)
)

*********************************************************************************
(defun sly(/ str1 m n pl SL ss) ͼ��༭
    (setq pl 0 n 10 s nil)
    (while (< pl n)
    (setq str1 (getstring "\nEntities LAYER name:"))
    (if (= str1 "") (progn (setq str1 "notlay") (setq pl (+ 9 pl)))
          (setq pl (+ 1 pl))) 
    (setq str1 (strcase str1))
    (SETQ S (cons str1 s))
     ;(setq pl (+ 1 pl)))
    );while
    (SETQ ss (ssget))
    (setq SL (sslength ss)) 
    (SETQ SSL (ssadd))
    (SETQ m 0)
    (while (< m SL)
       (setq e (ssname ss m))
       (setq edxf (entget e))
       (setq etab (cdr (assoc 8 edxf)))
       (foreach str1 s
       (if (= etab str1) (setq SSL (ssadd e SSL)))
       );for...
       (setq m (1+ m)));end while
    (command "select" SSL "")
    (princ (sslength ssl)) (princ " selected")
    (princ));end progn ;end if ;end sly...
(defun c:sy() (sly))
(defun c:my() (sly) (command "move" "p"""))  
(defun c:ey() (sly) (command "erase" "p""")) 
(defun c:cpy() (sly) (command "copy" "p""")) 
(defun c:cgy() (sly) (command "change" "p""""P")) 
(defun c:miy() (sly) (command "mirror" "p""")) 
(defun c:ary() (sly) (command "array" "p""")) 
(defun c:scy() (sly) (command "scale" "p""")) 
(defun c:roy() (sly) (command "rotate" "p"""))  

(DEFUN C:LS()
(SETQ LAYER(GETSTRING "INPUT LAYER NAME: "))
(COMMAND "LAYER""S" LAYER"")
)

(DEFUN C:CL(/ A LINE N INDEX B1 B C D B2 E)      
(SETQ LINE (GETSTRING "\nENTER LAYER NAME: "))
(SETQ E 0)
  (WHILE E
  (SETQ A (SSGET))
  (IF (= A NIL)(SETQ E NIL)
      (PROGN
      (SETQ N (SSLENGTH A))
      (SETQ INDEX 0)
      (REPEAT N
	  (SETQ B1 (ENTGET (SSNAME A INDEX)))
	  (SETQ INDEX (+ INDEX 1))
	  (SETQ C (ASSOC 8 B1))
	  (SETQ D (CONS 8 LINE))
	  (SETQ B2 (SUBST D C B1))
	  (ENTMOD B2)
      )
      )
   )   
   )
)

;****************4,ͼ�㸴�ƥ\��***********************************
(defun c:cx (/ os ce bm ss se la )
       (prompt "��ӭʹ�ò㸴�Ƴ���")
       (setq   os (getvar "osmode")
               ce (getvar "cmdecho")
       )
       (setvar "osmode" 3007)
       (setq se (getstring "\n input layer name<any key>/select matching object <enter>")
       )
       (if (= se "")
           (progn
           (setq la (cdr (assoc '8 (entget (car (entsel "\n select an object to match it"
           )        )    )         )       )    )
           (setq ss (ssget))
           (command "copy" ss "" "@" "@")
           (command "_change" ss "" "p" "la" la "")
           )
           (progn
           (setq la (getstring "\n enter layer name>"))
           (setq ss (ssget))
           (command "copy" ss "" "@" "@")
           (command "_change" ss "" "P" "la" la "")
           )
       )
       (setvar "osmode" os)
       (setvar "cmdecho" ce)
       (princ)
   )



;****************5,����ͼ��\��***********************************
(defun c:cgc (/ os ce bm ss se la )
             (prompt "��ӭʹ�ø���ͼ�����")
             (setq   os (getvar "OSMODE")
                     ce (getvar "CMDECHO")
                     bm (getvar "BLIPMODE"))                   
             (setvar "OSMODE" 3007)
             (setq se (getstring "\n Input layer name<any key>/Select matching object <enter>"))
             (if (= se "")
                 (progn
                 (setq la (cdr (assoc '8 (entget (car (entsel "\n select an object to match it"))))))
                 (setq ss (ssget))
                 (command "_change" ss "" "p" "la" la "")
                 )
                 (progn
                 (setq la (getstring "\n Enter layer name>"))
                 (setq ss (ssget))
                 (command "_change" ss "p" "la" la "")
                 )
          )
          (setvar "OSMODE" os)
          (setvar "CMDECHO" ce)
          (setvar "BLIPMODE" bm)
          (princ) 
)




;clone.lsp
;������ѡ������һ���㣬���ƶ���ԭʼλ�á�

(defun C:CF( / s l)
  (setvar "cmdecho" 0)
  (if
    (and
      (setq s (ssget))
      (not (zerop (sslength s)))
      (not (zerop (strlen (setq l (getstring "\nҪ���Ƶ��Ĳ㣺 "))))))
    (command "copy" s "" "@" "@" "change" s "" "p" "la" l "")
    (prompt "Invalid."))
  (progn)
)


; copylay.lsp
; ���Ƶ���ǰ�㣨copy_to_layer����
;�����򽫸���һ��ʵ�嵽��ǰ�����������ĸ��㡣

 (defun C:cpc ()
    (setvar "cmdecho" 0)
    (setq c_layer (getvar "clayer")
          sset (ssget)
          pt1 (getpoint "\n���㣺 ")
          count 0)
   (prompt "\nλ�Ƶĵڶ��㣺 ")
      (setq  len (sslength sset))
      (while (< count len)
       (setq name (ssname sset count)
             ptlst (entget name)
             b (assoc 8 ptlst)
             b1 (cdr (assoc 8 ptlst))
             c (cons 8 c_layer)
             d (subst c b ptlst)
             count (1+ count))
            (entmod d)
     )
    (command "_copy" sset "" pt1 pause)
    (setq count 0)
    (while (< count len)
    (setq name (ssname sset count)
             ptlst (entget name)
             b (assoc 8 ptlst)
             c (cons 8 b1)
             d (subst c b ptlst)
             count (1+ count))
            (entmod d)
     )
     (princ)
   )


;mlayer.lsp
;�ƶ��㣨move layer�����ƶ��ƶ����ϵ�����ʵ�塣

(DEFUN C:MLA()
(SETVAR "CMDECHO" 0)
(SETQ LAY (GETSTRING "\n����Ҫ�ƶ��Ĳ㣺 "))
(SETQ SS (SSGET "X" (LIST (CONS 8 LAY))))
(prompt "�����ƫ������ ")
(COMMAND "MOVE" SS ""  PAUSE)
(SETVAR "CMDECHO" 1)
(setq ss nil)
(PRINC)
)



;****************1,�����Υ\��********************************
(defun c:bx (/ os OO pt w l w2 l2 ptx pty pt1 pt2 pt3 pt4 )
        (setq os (getvar "osmode")
              OO (getvar "orthomode")
        )   ;
        (setvar "cmdecho" 0)
        (setvar "orthomode" 1)
        (setvar "osmode" 3007)
        (while
        (setq pt (getpoint "\n �������ĵ�>>"))
        (setvar "osmode" 0)
        (setq l (getdist "\n ���볤�� (x��):"))
        (setq w (getdist (strcat "\n ������ (Y��):<"
                                 (rtos l)
                                 ">"
        )       )        )
        (if (null w)
            (setq w l)
        )
        (setq w2 (/ w 2)
              l2 (/ l 2)
              ptx (car pt)
              pty (cadr pt)
              pt1 (list (+ ptx l2) (- pty w2))  
              pt2 (list (+ ptx l2) (+ pty w2))
              pt3 (list (- ptx l2) (+ pty w2))
              pt4 (list (- ptx l2) (- pty w2))
        )
        (command "Pline" pt1 pt2 pt3 pt4 "c")
        (setvar "osmode" 3007)
        )
        (setvar "osmode" os)
        (setvar "orthomode" OO)
        (princ)
        ) 


;****************������Ƕȯ�**********************************
(DEFUN C:hN (/ OS A PT1 PT2 ANG ANG1 ANG2)
       (SETVAR "CMDECHO" 0)
       (SETQ OS (GETVAR "OSMODE"))
       (SETVAR "OSMODE" 3007)
       (SETQ A (ENTGET (CAR (ENTSEL "\n ѡ��ǶȻ���"))))
       (SETQ PT1 (CDR (ASSOC '10 A
       )         )     )
       (SETQ PT2 (CDR (ASSOC '11 A
       )         )     )
       (SETQ ANG (ATOF (ANGTOS (ANGLE PT1 PT2) 0 4 
       )         )     )
       (SETQ ANG1 (GETREAL "\n ����Ƕ�>"))
       (SETQ ANG2 (+ ANG ANG1))
       (COMMAND "XLINE" "a" ANG2 PAUSE "")
       (SETVAR "OSMODE" OS)
       (PRINC)  ;
)


;****************���ش��***************************************
(defun c:bbb (/ os  obj pt   cir r);pt2
      (setvar "cmdecho" 0)
      (setq os (getvar "osmode"))
      (setvar "osmode" 0)
         (setq obj (car (entsel "\n ѡ������>>"
         )         )    )
         (setq pt2 (cdr (assoc 11 (entget obj
         )         )    )         )
         (setq pt2 (trans (list (car pt2) (cadr pt2) ) 0 1
         )         )
         (setq r (/ (getvar "viewsize") 40
         )       )
         (command "_circle" pt2 r)
         (setq cir (entlast))
         (setvar "osmode" 3007)
         (while
         (setq pt (getpoint "\n �Ի�Ȧ�Ķ˵�Ϊ��һ���ϵ㣬��ʼѡ��ڶ����ϵ�"))
         (setq pt (list (car pt) (cadr pt)))
         (command "break" obj pt pt)
         )
         (entdel cir)
         (setvar "osmode" os)
         (princ)
)

(DEFUN C:bK () 
   (PRINC "\n ���ɴ�� \(2005.8.5������\) ")
   (SETQ E 0)
   (WHILE E
    (SETQ A (SSGET))
    (IF (= A NIL)(SETQ E NIL)
      (PROGN 
      (SETQ P(GETPOINT"\nѡ���ϵ�:"))
      (COMMAND "BREAK" P P))
    )
   ) 
)

; zmballoon.lsp
;��������Ļ���ƶ�һ�����Ŵ󾵡�������Χ����ʵʱ�Ŵ�

(defun c:zb ()
    (PRINC "\n ��Ļ���Ŵ󾵡�\(2005.8.5������\) ")
    (setvar "cmdecho" 0)
    (command "layer" "new" "viewer" "")
    (setvar "clayer" "viewer")
    (setvar "cecolor" "yellow")
    (setq r (getdist "\n���ù۲�����ʰȡ��İ�����  "))
    (princ)
    (setq ip (list 3 3))
    (command "circle" ip "d" r )
    (setq ent (entlast))
    (setq stop 1)
    (setq d ip)
    (while (= stop 1)
       (command "move" ent "" d pause  )
       (setq d1 (getvar "lastpoint"))
       (setq r1 (/ r 2))
       (setq pt1 (polar d1 0 r1))
       (setq pt2 (polar d1 3.1415926 r1))
       (setq pt3 (polar pt1 4.712389 r1))
       (setq pt4 (polar pt2 1.570796 r1))
       (command "zoom" "window" pt3 pt4)
       (initget "y n")
       (setq ans (getkword "\n���������˳�/Y  "  ))
       (if(= ans "y")(setq stop 3))
       (command "zoom" "all" )
       (setq d (getvar "lastpoint" ))
    )
       (command "zoom" "all" )
       (command "erase" ent "")
       (setvar "clayer" "0")
       (setvar "cecolor" "bylayer")
       )

(DEFUN C:up ()(COMMAND "DIM" "UP" ))
(DEFUN C:TTT ()(COMMAND "CIRCLE" "3P" "TAN" PAUSE "TAN" PAUSE "TAN" PAUSE))
(DEFUN C:TTR ()(COMMAND "CIRCLE" "TTR" ))
(defun c:2P()(command "circle" "2p"))
(defun c:3P()(command "ARC" "3p"))
(DEFUN C:LL ()(COMMAND "line" "tan" PAUSE "tan" ))
(DEFUN C:tt() (COMMAND "line" pause "tan" pause ""))
(DEFUN C:DY ()(COMMAND "LENGTHEN" "DY" ))
(DEFUN C:TO ()(COMMAND "LENGTHEN" "TO" ))
(DEFUN C:DE ()(COMMAND "LENGTHEN" "DE" ))
(DEFUN C:SP ()(COMMAND "STRETCH" "CP" ))
(DEFUN C:ZA ()(COMMAND "zoom" "A" ))
(DEFUN C:ZE ()(COMMAND "ZOOM" "E" ))
(DEFUN C:ZV() (COMMAND "ZOOM" "V"))
(DEFUN C:ZD() (COMMAND "'ZOOM" "D"))
(DEFUN C:ZW() (COMMAND "ZOOM" "W"))
(DEFUN C:ZP() (COMMAND "ZOOM" "P")) 
(DEFUN C:UW() (COMMAND "UCS" "W"))
(DEFUN C:UB ()(COMMAND "UCS" "OB" ))
(defun c:uoo() (command "UCSICON" "ON" "UCS" "o" "int"))
(DEFUN C:UZ ()(COMMAND "UCS" "Z" ))
(DEFUN C:U5 ()(COMMAND "UCS" "Z" "5" ))
(DEFUN C:U10 ()(COMMAND "UCS" "Z" "10" ))
(DEFUN C:U15 ()(COMMAND "UCS" "Z" "15" ))
(DEFUN C:U20 ()(COMMAND "UCS" "Z" "20" ))
(DEFUN C:U30 ()(COMMAND "UCS" "Z" "30" ))
(DEFUN C:U45 ()(COMMAND "UCS" "Z" "45" ))
(DEFUN C:U60 ()(COMMAND "UCS" "Z" "60" ))
(DEFUN C:U70 ()(COMMAND "UCS" "Z" "70" ))
(DEFUN C:U3p ()(COMMAND "UCS" "3p" ))
(DEFUN C:HH ()(COMMAND "XLINE" "H" ))
(DEFUN C:VV ()(COMMAND "XLINE" "V" ))
(DEFUN C:FP ()(COMMAND "FILLET" "r" PAUSE)
              (COMMAND "FILLET" "p" PAUSE ))
(DEFUN C:f100 ()(COMMAND "FILLET" "R" "100" )(COMMAND "FILLET" ))
(DEFUN C:f20 ()(COMMAND "FILLET" "R" "20" )(COMMAND "FILLET" ))
(DEFUN C:f30 ()(COMMAND "FILLET" "R" "30" )(COMMAND "FILLET" ))
(DEFUN C:f40 ()(COMMAND "FILLET" "R" "40" )(COMMAND "FILLET" ))
(DEFUN C:f50 ()(COMMAND "FILLET" "R" "50" )(COMMAND "FILLET" ))
(DEFUN C:f60 ()(COMMAND "FILLET" "R" "60" )(COMMAND "FILLET" ))
(DEFUN C:f70 ()(COMMAND "FILLET" "R" "70" )(COMMAND "FILLET" ))
(DEFUN C:f80 ()(COMMAND "FILLET" "R" "80" )(COMMAND "FILLET" ))
(DEFUN C:f1 ()(COMMAND "FILLET" "R" "1" )(COMMAND "FILLET" ))
(DEFUN C:f2 ()(COMMAND "FILLET" "R" "2" )(COMMAND "FILLET" ))
(DEFUN C:f3 ()(COMMAND "FILLET" "R" "3" )(COMMAND "FILLET" ))
(DEFUN C:f4 ()(COMMAND "FILLET" "R" "4" )(COMMAND "FILLET" ))
(DEFUN C:f5 ()(COMMAND "FILLET" "R" "5" )(COMMAND "FILLET" ))
(DEFUN C:f6 ()(COMMAND "FILLET" "R" "6" )(COMMAND "FILLET" ))
(DEFUN C:f7 ()(COMMAND "FILLET" "R" "7" )(COMMAND "FILLET" ))
(DEFUN C:f8 ()(COMMAND "FILLET" "R" "8" )(COMMAND "FILLET" ))
(DEFUN C:f9 ()(COMMAND "FILLET" "R" "9" )(COMMAND "FILLET" ))
(DEFUN C:f10 ()(COMMAND "FILLET" "R" "10" )(COMMAND "FILLET" ))
(DEFUN C:Ch1 ()(COMMAND "CHAMFER" "D" "1" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch2 ()(COMMAND "CHAMFER" "D" "2" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch3 ()(COMMAND "CHAMFER" "D" "3" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch4 ()(COMMAND "CHAMFER" "D" "4" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch5 ()(COMMAND "CHAMFER" "D" "5" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch6 ()(COMMAND "CHAMFER" "D" "6" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch7 ()(COMMAND "CHAMFER" "D" "7" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch8 ()(COMMAND "CHAMFER" "D" "8" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch9 ()(COMMAND "CHAMFER" "D" "9" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch10 ()(COMMAND "CHAMFER" "D" "10" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch20 ()(COMMAND "CHAMFER" "D" "20" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch30 ()(COMMAND "CHAMFER" "D" "30" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch40 ()(COMMAND "CHAMFER" "D" "40" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch50 ()(COMMAND "CHAMFER" "D" "50" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch60 ()(COMMAND "CHAMFER" "D" "60" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch70 ()(COMMAND "CHAMFER" "D" "70" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch80 ()(COMMAND "CHAMFER" "D" "80" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch90 ()(COMMAND "CHAMFER" "D" "90" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch100 ()(COMMAND "CHAMFER" "D" "100" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch200 ()(COMMAND "CHAMFER" "D" "200" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch300 ()(COMMAND "CHAMFER" "D" "300" "")(COMMAND "CHAMFER" ))
(DEFUN C:Ch500 ()(COMMAND "CHAMFER" "D" "500" "")(COMMAND "CHAMFER" ))
(DEFUN C:m88 ()(COMMAND "insert" "m88" pause "" "" ""))
(DEFUN C:m66 ()(COMMAND "insert" "m66" pause "" "" ""))
(DEFUN C:m55 ()(COMMAND "insert" "m55" pause "" "" ""))
(DEFUN C:m44 ()(COMMAND "insert" "m44" pause "" "" ""))
(DEFUN C:L10 ()(COMMAND "insert" "L10" pause "" "" ""))
(DEFUN C:L8 ()(COMMAND "insert" "L8" pause "" "" ""))
(DEFUN C:L6 ()(COMMAND "insert" "L6" pause "" "" ""))
(DEFUN C:L5 ()(COMMAND "insert" "L5" pause "" "" ""))
(DEFUN C:L4 ()(COMMAND "insert" "L4" pause "" "" ""))
(DEFUN C:L3 ()(COMMAND "insert" "L3" pause "" "" ""))
(DEFUN C:DZ ()(COMMAND "insert" "DZ" pause "" "" ""))
(DEFUN C:FS ()(COMMAND "insert" "FS" pause "" "" ""))

(defun c:oe()(command "offset" pause pause pause "")
             (command "erase" "p" ""))
(defun c:od()(command "offset" pause pause pause "")
             (command "chprop" "l" "" "c" "BYLAYER" "lt" "dashed" ""))
(defun c:oj()(command "offset" pause pause pause "")
             (command "chprop" "l" "" "la" "o" ""))
(defun c:om()(command "offset" pause pause pause "")
             (command "chprop" "l" "" "la" "mm" ""))
(defun c:ol()(command "offset" pause pause pause "")
             (command "laycur" "l" ""))

(defun c:d0()(command "dimtoh" "0")(command "dim1" "up" pause ""))
(defun c:d1()(command "dimtoh" "1")(command "dim1" "up" pause ""))
(defun c:dn()(command "dim1" "n"))
(defun c:dl()(command "dim1" "l"))
(defun c:ltt()(command "_LEADER" pause pause  "A" "" "T"))
(defun c:lM()(command "_LEADER" pause pause pause "A" "" "" ""))
(defun c:lNN()(command "_LEADER" pause pause pause "A" "" "N"))

(DEFUN C:qs() (Command "_ZOOM" "E" "_PURGE" "a" "" "N" "LAYER" "ON" "*" "" "_qsave"))  
(DEFUN C:lP() (Command "LAYERP"))
(defun c:ds() (command "dimscale"))
(DEFUN C:dr() (COMMAND  "DIMORDINATE"))
(DEFUN C:df() (COMMAND "DIMLFAC")) 
(DEFUN C:DEC() (Command "DIMDEC"))
(DEFUN C:DAC() (Command "DIMADEC"))
(DEFUN C:11() (Command "osmode"))
(DEFUN C:XO() (Command "XLINE" "O" ))
(DEFUN C:cr() (Command "select" pause "" "copy" "p" "" "0,0" "" "rotate" "p" ""))
(DEFUN C:cm() (Command "select" pause "copy" "p" "" "m" pause))
(DEFUN C:Xa() (Command "XLINE" "a"))
(DEFUN C:Xb() (Command "XLINE" "b"))
(DEFUN C:UM() (Command "UNDO" "M" ))
(DEFUN C:UD() (Command "UNDO" "B" ))
(DEFUN C:mn() (Command "move" "l" "" "0,8" ""))
(DEFUN C:bp() (COMMAND "-boundary" pause "" )
              (COMMAND "chprop" "l" "" "la" pause ""
                       "erase"  "c"  pause pause "R"  "L" ))
(DEFUN C:EF() (SSGET) (Command "ERASE" "P" "R" "L" ""))
(DEFUN C:ds1() (COMMAND "dimstyle" "r" "1")) 
(DEFUN C:ds2() (COMMAND "dimstyle" "r" "2"))
(DEFUN C:ds3() (COMMAND "dimstyle" "r" "3"))
(DEFUN C:ds4() (COMMAND "dimstyle" "r" "4"))   
(DEFUN C:ly() (COMMAND "-LAYER" "ON" "*" "")) 
(DEFUN C:ln()  (COMMAND "-LAYER" "S" pause "OFF" "*" "" "")) 
(DEFUN C:lo() (COMMAND "-LAYER" "OFF" ))
(defun c:gm()(ssget)(command "chprop" "p" "" "la" "mm" ""))
(defun c:go()(ssget)(command "chprop" "p" "" "la" "o" ""))
(defun c:c3()(ssget)(command "chprop" "p" "" "c" "3" ""))
(defun c:c4()(ssget)(command "chprop" "p" "" "c" "4" ""))
(defun c:c5()(ssget)(command "chprop" "p" "" "c" "5" ""))
(defun c:c6()(ssget)(command "chprop" "p" "" "c" "6" ""))
(defun c:c7()(ssget)(command "chprop" "p" "" "c" "7" ""))
(defun c:c8()(ssget)(command "chprop" "p" "" "c" "8" ""))
(defun c:c250()(ssget)(command "chprop" "p" "" "c" "250" ""))
(defun c:po()(command "pedit" pause "" "j" "c" pause pause "" "")
             (command "offset" pause pause pause ""))
(defun c:pr()(command "pedit" pause "" "j" "c" pause pause "" "")
             (command "fillet" "r" pause)
             (command "fillet" "p" pause ))
(defun c:pj ()(command "pedit" pause "y" "j" "all" "" "")
(princ)
)
(defun c:ld()(command "linetype" "l" "center,divide,dashed" "" "")
             (command "ltscale" pause "")) ;�i�@һ���Ե��������ߣ����ߣ�˫�㻮��
(defun c:sk()(command "skpoly" "1")(command "sketch" "0.01"))
(defun c:aa()(command "area" "e"))
(defun C:tp() (command "dimtol" "1" "dimtp"))
(defun C:tm() (command "dimtol" "1" "dimtm"))
(defun C:tf() (command "dimtol" "0"))
(defun C:tn() (command "dimtol" "1" "DIMTFAC" "0.7"))



(defun c:ab( / d h B n DS CLAY PP1 DU DM TA DA) ;�Զ����������ı�����ԲȦ�ͱ�ʶ
   (SetQ DS (GETVAR "DIMSCALE"))
   (SetQ ORTH (GETVAR "ORTHOMODE"))
   (SETVAR "ORTHOMODE" 0)
   (setq d (/ DS 0.25))
   (setq h (/ DS 0.4))
   (PRINC "\n������һ���������:")
   (SETQ n (getint))
   (SETQ FHA n)
   (setq PP1 (getpoint "\nѡ���������ʼ��:"))
   (while (AND (/= pp1 nil))
     (progn
          (command "circle" pp1 "d" d  )
          (setq ta (ssget "L"))
	  (SETQ K1 FHA)
          (command "textstyle" "SIMPLE" )
          (command "text" "j"  "mc" pp1 h "" K1 )
          (setq Da (ssget "L"))
	  (SETQ FHA (+ FHA 1))
          (command "CHANGE" ta DA ""  "p" "La" "BH" "")
          (command "textstyle" "����" )
          (if (/= pp1 nil)
              (setq pp1 (getpoint pp1 "\nPrint start point:" ))
              (setq pp1 (getpoint "\nPrint start point:"))
              )
   )
 )
 (SETVAR "ORTHOMODE"  ORTH)
)


(defun c:db( / d h B n DS CLAY PP1 DU DM TA DA) ;�Զ��������ֻ���ĸ�ı�����ԲȦ�ͱ�ʶ
   (SetQ DS (GETVAR "DIMSCALE"))
   (setq d (/ DS 0.25))
   (setq h (/ DS 0.4))
   (IF (or (= FHA nil) (< FHA 65) (> FHA 90)) (SETQ FHA 65))
   (SETQ B (GETSTRING "/n���¿�ʼ����(Y/N):N"))
  (IF (OR (= B "Y") (= B "y"))
      (PROGN
        (PRINC "\n���뿪ʼ��ĸ(A~Z/1~9):") (PRINC  (CHR FHA))
        (SETQ n (ascii (STRCASE (GETSTRING))))
        (SETQ FHA n)
        ))
  (COND
        ((AND (> FHA 64) (< FHA 91))
         (PROGN
               (SETQ DM 64)
               (SETQ DU 91)
         ))
        ((AND (> FHA 48) (< FHA 58))
         (PROGN
               (SETQ DM 48)
               (SETQ DU 58)
         ))
  )
  (setq PP1 (getpoint "\nPrint start point:"))
  (while (AND (/= pp1 nil) (< FHA DU) (> FHA DM))
     (progn
          (command "circle" pp1 "d" d  "")
          (setq ta (ssget "L"))
          (command "textstyle" "simple" "" )
          (command "text" "j"  "mc" pp1 h "" (STRCASE (chr FHA)) "" )

          (setq Da (ssget "L"))
          (SETQ FHA (+ FHA 1))
          (command "textstyle" "����" )
          (command "CHANGE" ta DA ""  "p" "La" "BH"  "")
          (if (/= pp1 nil)
              (setq pp1 (getpoint pp1 "\nPrint start point:" ))
              (setq pp1 (getpoint "\nPrint start point:"))
              )
   )
 )
)



(defun c:ddd() ;�ֶ������ı�����ԲȦ�ͱ�ʶ
(SetQ DS (GETVAR "DIMSCALE"))
(setq d (/ DS 0.5))
(setq h (/ DS 0.4))
(setq p1 (getpoint "\nselect a point:"))
(command "circle" p1 d)
(setq ta (ssget "L"))
(setq A (getstring "please input s word:"))
(command "textstyle" "����" )
(command "text" "m" p1 h "0" A)
(setq Da (ssget "L"))
(command "CHANGE" ta DA ""  "p" "La" "BH"  "")
(command "textstyle" "����" )
)


(defun c:CD() ;�޸���ѡԲ��ֱ��
(princ "\n�޸���ѡԲ��ֱ����[������] ")
(setq ss (ssget))
(setq d (getreal "Բ�µ�ֱ��:"))
(setq r (/ d 2))
(setq n 0)
(setq c 0)
  (while (<= c (-  (sslength ss) 1))
  (setq e (ssname ss c))
  (command "change" e "" "" r)
  (setq c (+ 1 c))
  )
(princ)
)

(defun C:pej
  (/ SSET1)
  (princ "\n���˵����ͬ����x��y���꣬���Խ����ֱ��������һ���Ϊ�����ߡ�[������] ")
  (princ "\nѡ��Ҫ���ӵ��ߣ� ")
  (if 
     (setq SSET1 
        (ssget))
     (if 
        (= "POLYLINE"
           (cdr
              (assoc 0 
                 (entget 
                    (ssname SSET1 0)))
 )
)
              (command "PEDIT" SSET1 "J" SSET1 "" "X")
              (command
	"PEDIT" SSET1 "Y" "J" SSET1 "" "X"
 )
)
                 (princ "\nû��ѡ���ߡ� ")
)              (princ)
);end jn.lsp


;===============================================================
;�Ƕ����г���Angle array routine��,��AutoCAD���������׵öࡣ

(defun C:AAR ( / ss ang bp cnt dist d)
(PRINC "\n �Ƕ�����\(2005.8.5������\) ")
  (cond
    (  (setq ss (ssget))
       (initget 1)
       (setq bp (getpoint "\n���㣺 "))
       (initget 1)
       (setq ang
           (getangle bp "\n���з��� "))
         (initget 7)
         (setq dist
           (getdist "\n�����ࣺ "))
         (initget 7)
         (setq cnt (getint "\n��������� "))
         (setq d 0.0)
         (setvar "cmdecho" 0)
         (command "_.undo" "_g"
                  "_.copy" ss "" "_m" bp)
         (repeat (1- cnt)
           (command
               (polar bp ang (setq d (+ d dist)))))
               (command "" "_.undo" "_e")
        )
      )
      (princ)
)



