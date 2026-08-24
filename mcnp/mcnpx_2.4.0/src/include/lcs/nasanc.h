!     $Id: nasanc.h,v 1.4 2002/03/27 18:14:16 gwm Exp $                       !/*na          1*/
#ifdef OLDM                                                                     /*!na          2*/
      real nasan                                                                !/*na          3*/
#else                                                                           /*!na          4*/
      real*8 nasan                                                              !/*na          5*/
#endif                                                                          /*!na          6*/
!     CHANGE nasan to (64-bit) rnasan after tracking is OK.                     !/*na          7*/
#ifndef OLDM                                                                    /*!na          8*/
      parameter (iemxnn=26, iamxnn=12)                                          !/*na          9*/
#else                                                                           /*!na         10*/
      parameter (iemxnn=26, iamxnn=10)                                          !/*na         11*/
#endif                                                                          /*!na         12*/
      common /nasanc/ nasan(iemxnn,iamxnn), evalnn(iemxnn),                     !/*na         13*/
     & avalnn(iamxnn)                                                           !/*na         14*/
