!     $Id: getjdt.F,v 1.4 2002/03/27 18:16:25 gwm Exp $                       !/*ge          1*/
      subroutine getjdt(ha,hb)                                                  !/*ge          2*/
#ifdef AIX                                                                      /*!ge          3*/
!        required to make ibm-aix data and time call work.                      !/*ge          4*/
      character ha*36,hb*36                                                     !/*ge          5*/
      hb=ha                                                                     !/*ge          6*/
#endif                                                                          /*!ge          7*/
      return                                                                    !/*ge          8*/
      end                                                                       !/*ge          9*/
