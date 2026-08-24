!     $Id: qou19.F,v 1.4 2002/03/27 18:15:18 gwm Exp $                        !/*qo          1*/
      subroutine qou19                                                          !/*qo          2*/
! QOU19      calls...  QSTOR                                                    !/*qo          3*/
!     QOU19  called by...   ISOB                                                !/*qo          4*/
#include <lcs/double.h>                                                         /*!qo          5*/
#include <lcs/isobar.h>                                                         /*!qo          6*/
      pt_(3)=pt_(3)+((pt_(11)-pt_(12))/rcpmv_)                                  !/*qo          7*/
!     collision allowed                                                         !/*qo          8*/
      k=3                                                                       !/*qo          9*/
      go to 70                                                                  !/*qo         10*/
   10 i3_=-1                                                                    !/*qo         11*/
   20 return                                                                    !/*qo         12*/
   30 i2_=2                                                                     !/*qo         13*/
   40 i1_=(k/12)+3                                                              !/*qo         14*/
      call qstor                                                                !/*qo         15*/
      if (k-15) 50,60,90                                                        !/*qo         16*/
   50 k=15                                                                      !/*qo         17*/
      go to 30                                                                  !/*qo         18*/
   60 k=27                                                                      !/*qo         19*/
      pt_(27)=pt_(27)+((pnidk_(12)-pt_(k+1))/rcpmv_)                            !/*qo         20*/
   70 if (k-15) 30,80,80                                                        !/*qo         21*/
   80 i2_=0                                                                     !/*qo         22*/
      go to 40                                                                  !/*qo         23*/
   90 if (k-27) 10,100,110                                                      !/*qo         24*/
  100 if (pt_(39)) 110,110,120                                                  !/*qo         25*/
  110 i3_=0                                                                     !/*qo         26*/
      go to 20                                                                  !/*qo         27*/
  120 i2_=1                                                                     !/*qo         28*/
      k=39                                                                      !/*qo         29*/
      go to 40                                                                  !/*qo         30*/
      end                                                                       !/*qo         31*/
