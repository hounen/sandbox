!     $Id: srchn.F,v 1.4 2002/03/27 18:14:03 gwm Exp $                        !/*sr          1*/
      subroutine srchn (nca)                                                    !/*sr          2*/
! collision tape record types                                                   !/*sr          3*/
!     ncol ngo                                                                  !/*sr          4*/
!      -4   1   end of run                                                      !/*sr          5*/
!      -3   2   end of batch                                                    !/*sr          6*/
!      -2   3   end of tape (new tape to be assigned)                           !/*sr          7*/
!      -1   4   start of run                                                    !/*sr          8*/
!       0   5   not used                                                        !/*sr          9*/
!       1   6   source-particle data                                            !/*sr         10*/
!       2   7   nuclear interaction (nonabsorption)                             !/*sr         11*/
!       3   8   charged particle slowed to cutoff                               !/*sr         12*/
!       4   9   escape                                                          !/*sr         13*/
!       5  10   pseudo-collision                                                !/*sr         14*/
!       6  11   nuclear absorption                                              !/*sr         15*/
!       7  12   internal medium boundary crossing                               !/*sr         16*/
!       8  13   scattering                                                      !/*sr         17*/
#include <htape3x/com32b.h>                                                     /*!sr         18*/
#include <htape3x/label.h>                                                      /*!sr         19*/
#include <htape3x/contrl.h>                                                     /*!sr         20*/
#include <htape3x/srcwt.h>                                                      /*!sr         21*/
#include <htape3x/com10.h>                                                      /*!sr         22*/
#include <htape3x/com11.h>                                                      /*!sr         23*/
      real qq10                                                                 !/*sr         24*/
      dimension qq10(29), iqq11(15)                                             !/*sr         25*/
      equivalence (iqq10,qq10),(iqq11,qq11)                                     !/*sr         26*/
      dimension nc(8), nca(8), ncq(8)                                           !/*sr         27*/
      data ncq /1,7*0/                                                          !/*sr         28*/
      iflag=0                                                                   !/*sr         29*/
      go to 10                                                                  !/*sr         30*/
      entry srchnp(nca)                                                         !/*sr         31*/
      iflag=1                                                                   !/*sr         32*/
   10 do 20 i=1,8                                                               !/*sr         33*/
   20 nc(i)=nca(i)                                                              !/*sr         34*/
      go to 40                                                                  !/*sr         35*/
      entry rednmt                                                              !/*sr         36*/
      iflag=-1                                                                  !/*sr         37*/
      do 30 i=1,8                                                               !/*sr         38*/
   30 nc(i)=1                                                                   !/*sr         39*/
   40 continue                                                                  !/*sr         40*/
      if (nhst.eq.nh11.and.(ncol11.ne.-4.or..not.log11)) then                   !/*sr         41*/
        if (lf11a) then                                                         !/*sr         42*/
          call xtp11 (iflag,nc)                                                 !/*sr         43*/
        else                                                                    !/*sr         44*/
          go to 75                                                              !/*sr         45*/
        endif                                                                   !/*sr         46*/
   50   if (imerge.eq.0.and.fwton.and.ncol.ge.1.and.nocas.ne.nocasp)            !/*sr         47*/
     &  then                                                                    !/*sr         48*/
          nocasp=nocas                                                          !/*sr         49*/
          nhst=nh10                                                             !/*sr         50*/
   60     call tp10 (0,ncq)                                                     !/*sr         51*/
          if (ncol.lt.1.or.nocas.ne.nocasp) go to 60                            !/*sr         52*/
          nhst=nh11                                                             !/*sr         53*/
          if (fwti.le.dp0) then                                                 !/*sr         54*/
   70       if (lf11a) then                                                     !/*sr         55*/
              call xtp11 (iflag,nc)                                             !/*sr         56*/
            else                                                                !/*sr         57*/
              go to 75                                                          !/*sr         58*/
            endif                                                               !/*sr         59*/
            if (ncol.ge.1.and.nocas.eq.nocasp) go to 70                         !/*sr         60*/
            go to 50                                                            !/*sr         61*/
          endif                                                                 !/*sr         62*/
          log11=.false.                                                         !/*sr         63*/
          if (lf11a) then                                                       !/*sr         64*/
            call xtp11 (iflag,nc)                                               !/*sr         65*/
          else                                                                  !/*sr         66*/
            go to 75                                                            !/*sr         67*/
          endif                                                                 !/*sr         68*/
        endif                                                                   !/*sr         69*/
      elseif (nhst.eq.nh10.and.(ncol10.ne.-4.or..not.log10)) then               !/*sr         70*/
        call tp10 (iflag,nc)                                                    !/*sr         71*/
      endif                                                                     !/*sr         72*/
      if (imerge.eq.0) go to 45                                                 !/*sr         73*/
!     if (ncol.le.0.or.ncol10.le.0.or.ncol11.le.0) write (jtty,*)               !/*sr         74*/
!    & ' ncol10,ncol11,ncol ',ncol10,ncol11,ncol                                !/*sr         75*/
      if (ncol10.eq.-4.and.ncol11.eq.-4) then                                   !/*sr         76*/
        ncol=-4                                                                 !/*sr         77*/
!     write (jtty,*) ' set B ncol',ncol                                         !/*sr         78*/
      elseif (ncol10.eq.-4.and.ncol11.gt.0) then                                !/*sr         79*/
        nhst=nh11                                                               !/*sr         80*/
        go to 40                                                                !/*sr         81*/
      elseif (ncol11.eq.-4.and.ncol10.gt.0) then                                !/*sr         82*/
        nhst=nh10                                                               !/*sr         83*/
        go to 40                                                                !/*sr         84*/
      endif                                                                     !/*sr         85*/
   45 continue                                                                  !/*sr         86*/
      if (fwton.and.fwtcum.le.dp0.and.ncol.eq.-4) then                          !/*sr         87*/
        write (io,80)                                                           !/*sr         88*/
        write (jtty,80)                                                         !/*sr         89*/
        stop 'in srchn'                                                         !/*sr         90*/
      endif                                                                     !/*sr         91*/
      return                                                                    !/*sr         92*/
   75 continue                                                                  !/*sr         93*/
      call expire (0,'srchn','NEUTP format file not read')                      !/*sr         94*/
!                                                                               !/*sr         95*/
   80 format (///' original and modified sources effectively disjoint;'/        !/*sr         96*/
     & ' all tallies identically zero;')                                        !/*sr         97*/
      end                                                                       !/*sr         98*/
