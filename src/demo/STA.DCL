sta :dialog{
  label      = "��׼ģ��";
  spacer_0;
  : column {
     : row {
        : edit_box {
           label        ="������";
           key          ="order";
           alinment     =bottom;}
      }
     : row {
       : edit_box {
           label        = "�ͺ�:";
           key          = "type_p";
           edit_width   = 3;
           fixed_width  = true;}
        : edit_box {
           key          = "msize";
           edit_width   = 5;
           fixed_width  = true;}
        : edit_box {
           label        = "A:";
           key          = "apd";
           edit_width   = 3;
           fixed_width  = true;}
        : edit_box {
           label        = "B:";
           key          = "bpd";
           edit_width   = 3;
           fixed_width  = true;}
        : edit_box {
           label        = "S/Y:";
           key          = "s_y";
           edit_width   = 1;
           fixed_width  = true;}
        : edit_box {
           label        = "O/I:";
           key          = "o_i";
           edit_width   = 1;
           fixed_width  = true;}
      }
     : boxed_radio_row {
        key         = "egy_n";
        : text {
            label       = "��׼λ�ü����ж�/˾:";
            fix_height  = ture;
            width       = 15;}
        : radio_button {
            label       = "��_";
            key         = "N";
            alignment   = bottom;}
        : radio_button {
	    label       = "��";
            key         = "Y";
            alignment   = bottom;}
        } 
      ok_cancel;
  } }
        