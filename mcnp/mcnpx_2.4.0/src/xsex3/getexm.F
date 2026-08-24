!#ident getexm.F from deck ge                                                   !/*ge          1*/
      subroutine getexm(hm)                                                     !/*ge          2*/
!        put the execute line message into hm.                                  !/*ge          3*/
#ifdef HPUX                                                                     /*!gexa        1*/
#ifndef G77                                                                     /*!gexa        2*/
$hp9000_800 system on                                                           !/*gexa        3*/
#endif                                                                          /*!gexa        4*/
#endif                                                                          /*!gexa        5*/
      character hm*80                                                           !/*ge          4*/
!                                                                               !/*ge          5*/
#ifdef VMS                                                                      /*!ge4c        6*/
      call lib$get_foreign(hm,,)                                                !/*ge         20*/
#endif                                                                          /*!cpp       538*/
#ifdef UNICOS                                                                   /*!ge4b        1*/
      j=0                                                                       !/*ge4b        2*/
      do 10 i=1,ipxfargc()                                                      !/*ge4b        3*/
      call pxfgetarg(i,hm(j+1:),l,ie)                                           !/*ge4b        4*/
   10 j=j+l+1                                                                   !/*ge4b        5*/
#endif                                                                          /*!cpp       539*/
#if defined(UNIX) || defined(DEC)                                               /*!ge4c        7*/
      integer*4 ia                                                              !/*ge4c        8*/
      hm=' '                                                                    !/*ge         51*/
      j=0                                                                       !/*ge         52*/
#ifdef GENERIC_ENV                                                              /*!gexl        1*/
      call generic_iargc(itmp)                                                  !/*gexl        2*/
      do 10 i=1,itmp                                                            !/*gexl        3*/
      call generic_getarg(i,hm(j+1:))                                           !/*gexl        4*/
   10 j=j+leng(hm(j+1:))+1                                                      !/*gexl        5*/
#else                                                                           /*!gexl        6*/
      do 10 i=1,iargc()                                                         !/*ge         53*/
      ia=i                                                                      !/*ge4c        9*/
      call getarg(ia,hm(j+1:))                                                  !/*ge4c       10*/
   10 j=j+leng(hm(j+1:))+1                                                      !/*ge         55*/
#endif                                                                          /*!gexl        7*/
#endif                                                                          /*!cpp       540*/
#ifdef PCDOS                                                                    /*!ge4a        8*/
      call getcl(hm)                                                            !/*ge4a        9*/
#endif                                                                          /*!cpp       544*/
#ifdef HPUX                                                                     /*!gexa        6*/
#ifndef G77                                                                     /*!gexa        7*/
$hp9000_800 system off                                                          !/*gexa        8*/
#endif                                                                          /*!gexa        9*/
#endif                                                                          /*!gexa       10*/
      return                                                                    !/*ge         57*/
      end                                                                       !/*ge         58*/
