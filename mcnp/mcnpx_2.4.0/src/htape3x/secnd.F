!     $Id: secnd.F,v 1.6 2002/03/27 18:14:03 gwm Exp $                        !/*se          1*/
      subroutine secnd (t)                                                      !/*se          2*/
!        return the time consumed so far in this run, in seconds.               !/*se          3*/
#ifdef CHEAP                                                                    /*!se          4*/
      implicit double precision (a-h,o-z)                                       !/*se          5*/
#endif                                                                          /*!se          6*/
      real etime,a2(2)                                                          !/*se          7*/
#ifdef PCDOS                                                                    /*!se          8*/
      save t0,tl                                                                !/*se          9*/
      data t0,tl/100000.,0./                                                    !/*se         10*/
#endif                                                                          /*!se         11*/
!                                                                               !/*se         12*/
      t=0.                                                                      !/*se         13*/
#ifdef UNICOS                                                                   /*!se         14*/
#ifndef T3D                                                                     /*!se         15*/
      call second(t)                                                            !/*se         16*/
#else                                                                           /*!se         17*/
      call tsecnd(t)                                                            !/*se         18*/
#endif                                                                          /*!se         19*/
#endif                                                                          /*!se         20*/
#ifdef UNIX                                                                     /*!se         21*/
!                                                                               !/*se         22*/
!     TEMPORARY expedient: HTAPE3X and XSEX3 do not report times.               !/*se         23*/
!     This will be corrected when a proper modularization of the                !/*se         24*/
!     etime() function has been achieved for other than MCNPF.                  !/*se         25*/
!     t=etime(a2)                                                               !/*se         26*/
!                                                                               !/*se         27*/
#endif                                                                          /*!se         28*/
#ifdef PCDOS                                                                    /*!se         29*/
!        only good if this routine is called at least once a day.               !/*se         30*/
      call timer(i)                                                             !/*se         31*/
      t=.01*i                                                                   !/*se         32*/
      if(t0.eq.100000.)t0=t                                                     !/*se         33*/
      if(t.lt.tl)t0=t0-86400.                                                   !/*se         34*/
      tl=t                                                                      !/*se         35*/
      t=t-t0                                                                    !/*se         36*/
#endif                                                                          /*!se         37*/
      return                                                                    !/*se         38*/
      end                                                                       !/*se         39*/
