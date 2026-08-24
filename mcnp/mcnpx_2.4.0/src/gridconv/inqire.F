!     $Id: inqire.F,v 1.4 2002/03/27 18:13:52 gwm Exp $                       !/*in          1*/
      function inqire(hf)                                                       !/*in          2*/
!        return 1 if file hf exists, 0 if it does not.                          !/*in          3*/
      character hf*(*)                                                          !/*in          4*/
      logical lv                                                                !/*in          5*/
!                                                                               !/*in          6*/
      inqire=1                                                                  !/*in          7*/
      inquire(file=hf,exist=lv)                                                 !/*in          8*/
      if(.not.lv)inqire=0                                                       !/*in          9*/
      return                                                                    !/*in         10*/
      end                                                                       !/*in         11*/
