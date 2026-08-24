      subroutine derig (ncas, intel)                                            !/*de          1*/
#ifdef CEM                                                                      /*!de          2*/
! ======================================================================        !/*de          3*/
!                                                                               !/*de          4*/
!   Prints out a table of emitted particle properties for the first nnnp        !/*de          5*/
!   reactions.                                                                  !/*de          6*/
!                                                                               !/*de          7*/
!   CEM95 written by S. G. Mashnik                                              !/*de          8*/
!                                                                               !/*de          9*/
!   Edited by A. J. Sierk  LANL  T-2  February, 1996.                           !/*de         10*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*de         11*/
!                                                                               !/*de         12*/
! ======================================================================        !/*de         13*/
#include <cem/cdouble.h>                                                        /*!de         14*/
#include <lcs/inout.h>                                                          /*!de         15*/
      common /bl1003/ ut, at, zt                                                !/*de         16*/
      common /blok77/ spt(5,150)                                                !/*de         17*/
      common /degrad/ degrad                                                    !/*de         18*/
      common /kktot/  ktot                                                      !/*de         19*/
      common /zapp/   parz(6,150)                                               !/*de         20*/
! ======================================================================        !/*de         21*/
      write (io, 100) ncas, intel                                               !/*de         22*/
      write (itty, 100) ncas, intel                                             !/*de         23*/
      write (io, 200) at, zt, ut, ktot                                          !/*de         24*/
      write (itty, 200) at, zt, ut, ktot                                        !/*de         25*/
      write (io, 300)                                                           !/*de         26*/
      write (itty, 300)                                                         !/*de         27*/
        do 10 k = 1,ktot                                                        !/*de         28*/
        tet = parz(3,k)*degrad                                                  !/*de         29*/
        fi = parz(4,k)*degrad                                                   !/*de         30*/
        t = parz(2,k)*dp1000                                                    !/*de         31*/
        am = spt(5,k)*dp1000                                                    !/*de         32*/
        qq = parz(6,k)                                                          !/*de         33*/
        write(io,400)parz(1,k),qq,t,tet,fi,parz(5,k),spt(1,k),                  !/*de         34*/
     &              spt(2,k), am                                                !/*de         35*/
        write(itty,400)parz(1,k),qq,t,tet,fi,parz(5,k),spt(1,k),                !/*de         36*/
     &              spt(2,k), am                                                !/*de         37*/
   10   continue                                                                !/*de         38*/
! ======================================================================        !/*de         39*/
  100 format(/1x,'ncas =',i7,', intel =',i7)                                    !/*de         40*/
  200 format(1x,'At = ',f4.0,', Zt =',f4.0,', Ut =',f4.0,', ktot =',i3)         !/*de         41*/
  300 format(1x,'Par',1x,'Q',3x,'T(MeV)',1x,'thet',3x,'phi',3x,                 !/*de         42*/
     &       'ngen',4x,'st',4x,'ct',4x,'M(MeV)'/)                               !/*de         43*/
  400 format(1x,f2.0,1x,f3.0,1x,f6.1,2x,f4.0,2x,f4.0,2x,f5.0,2x,f5.3,           !/*de         44*/
     &       2x,f5.3,2x,f6.1)                                                   !/*de         45*/
! ======================================================================        !/*de         46*/
#endif                                                                          /*!de         47*/
      return                                                                    !/*de         48*/
      end                                                                       !/*de         49*/
