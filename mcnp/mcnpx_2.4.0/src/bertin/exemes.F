!     $Id: exemes.F,v 1.4 2002/03/27 18:13:49 gwm Exp $                       !/*ex          1*/
      subroutine exemes (isub,msub,ndef)                                        !/*ex          2*/
!        interpret the execute line message                                     !/*ex          3*/
#include <htape3x/com32b.h>                                                     /*!ex          4*/
      character *8 isub(ndef)                                                   !/*ex          5*/
      character exms*80, msub(ndef)*8                                           !/*ex          6*/
      character hm(64)*10                                                       !/*ex          7*/
      data exms /' '/                                                           !/*ex          8*/
      data iuou /0/                                                             !/*ex          9*/
#ifdef UNIX                                                                     /*!ex         10*/
      data jtty /6/                                                             !/*ex         11*/
#endif                                                                          /*!ex         12*/
#ifndef UNIX                                                                    /*!ex         13*/
      data jtty /59/                                                            !/*ex         14*/
#endif                                                                          /*!ex         15*/
!                                                                               !/*ex         16*/
      jr=0                                                                      !/*ex         17*/
      kb=0                                                                      !/*ex         18*/
      kr=0                                                                      !/*ex         19*/
!                                                                               !/*ex         20*/
!        do the execute line message and the inp message block.                 !/*ex         21*/
      do 10 i=1,64                                                              !/*ex         22*/
   10 hm(i)=' '                                                                 !/*ex         23*/
      call getexm (exms)                                                        !/*ex         24*/
!                                                                               !/*ex         25*/
!        set up to interpret the execute line message.                          !/*ex         26*/
      call ckchar (exms,jtty,iuou)                                              !/*ex         27*/
      j=0                                                                       !/*ex         28*/
      do 20 ni=0,62                                                             !/*ex         29*/
      call nxtsym (exms,' =',j+1,i,j)                                           !/*ex         30*/
      if (j.eq.0) go to 30                                                      !/*ex         31*/
   20 hm(ni+1)=exms(i:j)                                                        !/*ex         32*/
      go to 90                                                                  !/*ex         33*/
!                                                                               !/*ex         34*/
!        interpret the message symbols.                                         !/*ex         35*/
   30 ij=1                                                                      !/*ex         36*/
      do 70 js=1,ni                                                             !/*ex         37*/
      if (js.lt.ij) go to 70                                                    !/*ex         38*/
!                                                                               !/*ex         39*/
!        change a file name if the next symbol is =.                            !/*ex         40*/
      if (hm(js+1).ne.'=') go to 60                                             !/*ex         41*/
      do 40 j=1,ndef                                                            !/*ex         42*/
   40 if (msub(j).eq.hm(js)) go to 50                                           !/*ex         43*/
      call expire (0,'exemes','file name '//hm(js)//                            !/*ex         44*/
     & ' in the execution message is not recognized.')                          !/*ex         45*/
   50 isub(j)=hm(js+2)                                                          !/*ex         46*/
      i=index(hm(js+2),' ')                                                     !/*ex         47*/
      if (i.eq.0.or.i.gt.9) call expire (0,'exemes',                            !/*ex         48*/
     & 'too many characters in file name '//hm(js+2)//'  maximum is 8.')        !/*ex         49*/
      ij=js+3                                                                   !/*ex         50*/
      go to 70                                                                  !/*ex         51*/
!                                                                               !/*ex         52*/
   60 continue                                                                  !/*ex         53*/
!                                                                               !/*ex         54*/
   70 continue                                                                  !/*ex         55*/
!        set up default values for unspecified file names.                      !/*ex         56*/
      do 80 i=1,ndef                                                            !/*ex         57*/
   80 if (isub(i).eq.' '.and.msub(i).ne.'com') isub(i)=msub(i)                  !/*ex         58*/
      return                                                                    !/*ex         59*/
!                                                                               !/*ex         60*/
!        quit if anything is wrong in a message.                                !/*ex         61*/
   90 call expire (0,'exemes',                                                  !/*ex         62*/
     & 'too many symbols in the execution message.')                            !/*ex         63*/
      end                                                                       !/*ex         64*/
