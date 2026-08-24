!#ident lkon.h from comdeck lkon                                                !/*lkon        1*/
!     turn on multitasking lock.                                                !/*lkonx       1*/
!                                                                               !/*lk4c        1*/
#ifdef MULTT                                                                    /*!lk4c        2*/
      if(jlock.eq.1)then                                                        !/*lk4c        3*/
         jlock=2                                                                !/*lk4c        4*/
      else if(jlock.eq.-1)then                                                  !/*lk4c        5*/
         wz=wclock()                                                            !/*lk4c        6*/
#ifdef DEC                                                                      /*!lk4c        7*/
         call mp_setlock()                                                      !/*lk4c        8*/
#endif                                                                          /*!cpp        40*/
#ifdef UNICOS                                                                   /*!lk4c        9*/
         call lockon(locki)                                                     !/*lk4c       10*/
#endif                                                                          /*!cpp        41*/
         jlock=1                                                                !/*lk4c       11*/
         pptme(1)=pptme(1)+wclock()-wz                                          !/*lk4c       12*/
      endif                                                                     !/*lk4c       13*/
#endif                                                                          /*!cpp        39*/
