!     $Id: chgmem.F,v 1.5 2002/03/27 18:16:23 gwm Exp $                       !/*ch          1*/
      subroutine chgmem(ms,lf,ll,ha)                                            !/*ch          2*/
!        change length of dynamically allocated storage from lf to ll.          !/*ch          3*/
!        return with lf=ll.  ha is the location of the call.                    !/*ch          4*/
!        ms is mdas, the declared length of das.                                !/*ch          5*/
#ifdef XSEXPOINTERS                                                             /*!ch          6*/
      common /dac/ kdy                                                          !/*ch          7*/
#endif                                                                          /*!ch          8*/
      character ht*35,ha*8                                                      !/*ch          9*/
!                                                                               !/*ch         10*/
      ie=0                                                                      !/*ch         11*/
#ifdef XSEXPOINTERS                                                             /*!ch         12*/
#ifndef CHEAP                                                                   /*!ch         13*/
      call reallf(kdy,8*ll)                                                     !/*ch         14*/
#endif                                                                          /*!ch         15*/
#ifdef CHEAP                                                                    /*!ch         16*/
      call reallf(kdy,4*ll)                                                     !/*ch         17*/
#endif                                                                          /*!ch         18*/
      if(kdy.le.0)ie=1                                                          !/*ch         19*/
#endif                                                                          /*!ch         20*/
      if(ie.eq.0)go to 10                                                       !/*ch         21*/
      write(ht,'(6h   ie=,i3,5h  lf=,i8,5h  ll=,i8)')ie,lf,ll                   !/*ch         22*/
      call errorf ('chgmem.1')                                                  !/*ch         23*/
   10 lf=ll                                                                     !/*ch         24*/
#ifndef XSEXPOINTERS                                                            /*!ch         25*/
!                                                                               !/*ch         26*/
!        quit if the length of das is inadequate.                               !/*ch         27*/
      if(ll.gt.ms) call errorf ('chgmem.2')                                     !/*ch         28*/
#endif                                                                          /*!ch         29*/
      return                                                                    !/*ch         30*/
      end                                                                       !/*ch         31*/
