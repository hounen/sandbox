!#ident lkoff.h from comdeck lkoff                                              !/*lkoff       1*/
!     turn off multitasking lock.                                               !/*lkoffx      1*/
!                                                                               !/*lk4c       15*/
#ifdef MULTT                                                                    /*!lk4c       16*/
      if(jlock.eq.1)then                                                        !/*lk4c       17*/
#ifdef DEC                                                                      /*!lk4c       18*/
         call mp_unsetlock()                                                    !/*lk4c       19*/
#endif                                                                          /*!cpp        43*/
#ifdef UNICOS                                                                   /*!lk4c       20*/
         call lockoff(locki)                                                    !/*lk4c       21*/
#endif                                                                          /*!cpp        44*/
         jlock=-1                                                               !/*lk4c       22*/
      else if(jlock.eq.2)then                                                   !/*lk4c       23*/
         jlock=1                                                                !/*lk4c       24*/
      endif                                                                     !/*lk4c       25*/
#endif                                                                          /*!cpp        42*/
