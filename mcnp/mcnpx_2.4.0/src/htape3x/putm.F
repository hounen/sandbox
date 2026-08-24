!     $Id: putm.F,v 1.3 2002/03/27 18:14:02 gwm Exp $                         !/*pu          1*/
      subroutine putm (aname,lsize,ityp,iptr)                                   !/*pu          2*/
#include <htape3x/com32b.h>                                                     /*!pu          3*/
      character *6 aname                                                        !/*pu          4*/
#include <htape3x/alloc.h>                                                      /*!pu          5*/
!                                                                               !/*pu          6*/
!     ITYP = 4 : single precision                                               !/*pu          7*/
!     ITYP = 8 : double precision                                               !/*pu          8*/
!     This version allocates s.p. words in a s.p. container only                !/*pu          9*/
!                                                                               !/*pu         10*/
      ict=ict+1                                                                 !/*pu         11*/
      if (ict.eq.num.or.ityp.ne.4) ierr=1                                       !/*pu         12*/
      names(ict)=aname                                                          !/*pu         13*/
      len(ict)=lsize                                                            !/*pu         14*/
      iptt(ict+1)=iptt(ict)+lsize                                               !/*pu         15*/
      iptr=iptt(ict)                                                            !/*pu         16*/
      if (iptt(ict+1)-1.ge.maxblk) ierr=1                                       !/*pu         17*/
      return                                                                    !/*pu         18*/
      end                                                                       !/*pu         19*/
