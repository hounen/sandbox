!#ident inqire.F from deck iq                                                   !/*iq          1*/
      function inqire(hf)                                                       !/*iq          2*/
!        return 1 if file hf exists, 0 if it does not.                          !/*iq          3*/
      character hf*(*)                                                          !/*iq          4*/
      logical lv                                                                !/*iq          5*/
!                                                                               !/*iq          6*/
      inqire=1                                                                  !/*iq          7*/
      inquire(file=hf,exist=lv)                                                 !/*iq          9*/
      if(.not.lv)inqire=0                                                       !/*iq         10*/
      return                                                                    !/*iq         11*/
      end                                                                       !/*iq         12*/
