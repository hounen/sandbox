!     $Id: exemes.F,v 1.4 2002/03/27 18:16:22 gwm Exp $                       !/*ex          1*/
      subroutine exemes (isub,msub,ndef)                                        !/*ex          2*/
!        interpret the execute line message                                     !/*ex          3*/
#include <htape3x/com32b.h>                                                     /*!ex          4*/
      character *8 isub(ndef)                                                   !/*ex          5*/
      character exms*80, msub(ndef)*8                                           !/*ex          6*/
      character hm(64)*10                                                       !/*ex          7*/
      data exms /' '/                                                           !/*ex          8*/
!cc                                                                             !/*ex          9*/
      data iuou /0/                                                             !/*ex         10*/
#ifdef UNIX                                                                     /*!ex         11*/
      data jtty /6/                                                             !/*ex         12*/
#endif                                                                          /*!ex         13*/
#ifndef UNIX                                                                    /*!ex         14*/
      data jtty /59/                                                            !/*ex         15*/
#endif                                                                          /*!ex         16*/
!cc                                                                             !/*ex         17*/
!                                                                               !/*ex         18*/
      jr=0                                                                      !/*ex         19*/
      kb=0                                                                      !/*ex         20*/
      kr=0                                                                      !/*ex         21*/
!                                                                               !/*ex         22*/
!        do the execute line message and the inp message block.                 !/*ex         23*/
      do 10 i=1,64                                                              !/*ex         24*/
   10 hm(i)=' '                                                                 !/*ex         25*/
      call getexm (exms)                                                        !/*ex         26*/
!                                                                               !/*ex         27*/
!        set up to interpret the execute line message.                          !/*ex         28*/
      call ckchar (exms,jtty,iuou)                                              !/*ex         29*/
      j=0                                                                       !/*ex         30*/
      do 20 ni=0,62                                                             !/*ex         31*/
      call nxtsym (exms,' =',j+1,i,j)                                           !/*ex         32*/
      if (j.eq.0) go to 30                                                      !/*ex         33*/
   20 hm(ni+1)=exms(i:j)                                                        !/*ex         34*/
      go to 90                                                                  !/*ex         35*/
!                                                                               !/*ex         36*/
!        interpret the message symbols.                                         !/*ex         37*/
   30 ij=1                                                                      !/*ex         38*/
      do 70 js=1,ni                                                             !/*ex         39*/
      if (js.lt.ij) go to 70                                                    !/*ex         40*/
!                                                                               !/*ex         41*/
!        change a file name if the next symbol is =.                            !/*ex         42*/
      if (hm(js+1).ne.'=') go to 60                                             !/*ex         43*/
      do 40 j=1,ndef                                                            !/*ex         44*/
   40 if (msub(j).eq.hm(js)) go to 50                                           !/*ex         45*/
      call expire (0,'exemes','file name '//hm(js)//                            !/*ex         46*/
     & ' in the execution message is not recognized.')                          !/*ex         47*/
   50 isub(j)=hm(js+2)                                                          !/*ex         48*/
      i=index(hm(js+2),' ')                                                     !/*ex         49*/
      if (i.eq.0.or.i.gt.9) call expire (0,'exemes',                            !/*ex         50*/
     & 'too many characters in file name '//hm(js+2)//'  maximum is 8.')        !/*ex         51*/
      ij=js+3                                                                   !/*ex         52*/
      go to 70                                                                  !/*ex         53*/
!                                                                               !/*ex         54*/
   60 continue                                                                  !/*ex         55*/
!                                                                               !/*ex         56*/
   70 continue                                                                  !/*ex         57*/
!        set up default values for unspecified file names.                      !/*ex         58*/
      do 80 i=1,ndef                                                            !/*ex         59*/
   80 if (isub(i).eq.' '.and.msub(i).ne.'com') isub(i)=msub(i)                  !/*ex         60*/
      return                                                                    !/*ex         61*/
!                                                                               !/*ex         62*/
!        quit if anything is wrong in a message.                                !/*ex         63*/
   90 call expire (0,'exemes',                                                  !/*ex         64*/
     & 'too many symbols in the execution message.')                            !/*ex         65*/
      end                                                                       !/*ex         66*/
