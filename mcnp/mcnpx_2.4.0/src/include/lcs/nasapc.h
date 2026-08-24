!     $Id: nasapc.h,v 1.4 2002/03/27 18:14:16 gwm Exp $                       !/*na          1*/
#ifdef OLDM                                                                     /*!na          2*/
      real nasap                                                                !/*na          3*/
#else                                                                           /*!na          4*/
      real*8 nasap                                                              !/*na          5*/
#endif                                                                          /*!na          6*/
!     CHANGE nasap to (64-bit) rnasap after tracking is OK.                     !/*na          7*/
#ifndef OLDM                                                                    /*!na          8*/
      parameter (iemxnp=37, iamxnp=11)                                          !/*na          9*/
#else                                                                           /*!na         10*/
      parameter (iemxnp=26, iamxnp=10)                                          !/*na         11*/
#endif                                                                          /*!na         12*/
      common /nasapc/ nasap(iemxnp,iamxnp), evalnp(iemxnp),                     !/*na         13*/
     & avalnp(iamxnp)                                                           !/*na         14*/
