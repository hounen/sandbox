      function wamm(a,z,il,ioption)                                             !/*wa          1*/
! ======================================================================        !/*wa          2*/
!                                                                               !/*wa          3*/
!   Calculates the ratio of the fission level density paramters at the          !/*wa          4*/
!   saddle point and the compound leveldensity parameter:                       !/*wa          5*/
!        ioption=1: W.Reisdorf, Z.Phys.A 300, 227-238 (1981)                    !/*wa          6*/
!        ioption=2: J.Toke, W.J.Swiatecki, Nuc. Phys. A372,                     !/*wa          7*/
!                   141-150 (1981)                                              !/*wa          8*/
!   remark: ioption=2 seems to give better values                               !/*wa          9*/
!                                                                               !/*wa         10*/
!   called by PRECOF                                                            !/*wa         11*/
!                                                                               !/*wa         12*/
!   calls: GETBSBK                                                              !/*wa         13*/
!                                                                               !/*wa         14*/
!   CEM95 written by S. G. Mashnik                                              !/*wa         15*/
!                                                                               !/*wa         16*/
!   Written by F. Gallmeier, ORNL CPED, July 1998                               !/*wa         17*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998                  !/*wa         18*/
!                                                                               !/*wa         19*/
! ======================================================================        !/*wa         20*/
#include <cem/cdouble.h>                                                        /*!wa         21*/
#ifndef CEM                                                                     /*!wa         22*/
      wamm=dp0                                                                  !/*wa         23*/
#else                                                                           /*!wa         24*/
#include <lcs/inout.h>                                                          /*!wa         25*/
      fissility(a,z) = z*z/a/(dp5088m2*(dp1-1.7826d0*((a-dp2*z)/a)**2))         !/*wa         26*/
!                                                                               !/*wa         27*/
!       deformation paramater = 1 - fissility                                   !/*wa         28*/
      y = dp1- max(dp0,min(dp1,fissility(a,z)))                                 !/*waa         1*/
!                                                                               !/*wa         30*/
!       bs und bk parameter                                                     !/*wa         31*/
      call getbsbk(y,bs,bk)                                                     !/*wa         32*/
!                                                                               !/*wa         33*/
!       choose options                                                          !/*wa         34*/
!        ioption=1: W.Reisdorf, Z.Phys.A 300, 227-238 (1981)                    !/*wa         35*/
!        ioption=2: J.Torke, W.J.Swiatecki, Nuc. Phys. A372,                    !/*wa         36*/
!                   141-150 (1981)                                              !/*wa         37*/
      if (ioption.eq.1) then                                                    !/*wa         38*/
!                                                                               !/*wa         39*/
!       constants                                                               !/*wa         40*/
!        nucleus charge radius (fm)                                             !/*wa         41*/
         r0  = 1.15d0                                                           !/*wa         42*/
!        Fermi vektor (1/fm)                                                    !/*wa         43*/
         fkf = 1.36d0                                                           !/*wa         44*/
!        hbar^2/2/m_neutron (MeV*fm^2)                                          !/*wa         45*/
         f   = (1.05457d-34*3.0d8*1.0d15/1.602d-13)**2                          !/*wa         46*/
         f   = dph*f/939.5656d0                                                 !/*wa         47*/
!        Fermi energy (MeV)                                                     !/*wa         48*/
         ef  = f*fkf*fkf                                                        !/*wa         49*/
         pi  = 3.1415926535898d0                                                !/*wa         50*/
!        nucleonic binding energy MeV                                           !/*wa         51*/
         bnd=15.0d0                                                             !/*wa         52*/
!                                                                               !/*wa         53*/
!       delta                                                                   !/*wa         54*/
         d=atan(-sqrt(bnd/ef))                                                  !/*wa         55*/
!                                                                               !/*wa         56*/
!       calculate af and an                                                     !/*wa         57*/
         a1 = r0*r0*a*fkf*dpth                                                  !/*wa         58*/
         a2 = r0*(dp25m2*pi - d)*a**dp2th                                       !/*wa         59*/
         a3 = a**(dpth)/fkf*(dpth +(cos(d))**2 +d/tan(d))                       !/*wa         60*/
         a4 = dp4*pi*r0/dp6/f                                                   !/*wa         61*/
         af = (a1 + bs*a2 +bk*a3)*a4                                            !/*wa         62*/
         an = (a1 + a2 +a3)*a4                                                  !/*wa         63*/
         wamm = af/an                                                           !/*wa         64*/
!         write(io,100) a,z,il,bs,bs0,bs40,bk,a1*a4,a2*a4,a3*a4,a4,             !/*wa         65*/
!     &             af,an,ann,wamm                                              !/*wa         66*/
      elseif (ioption.eq.2) then                                                !/*wa         67*/
         a1 = a                                                                 !/*wa         68*/
         a2 = dp3114m3*a**dp2th                                                 !/*wa         69*/
         a3 = dp5626m3*a**dpth                                                  !/*wa         70*/
         a4 = dp1/dp1461m2                                                      !/*wa         71*/
         af = (a1 + bs*a2 +bk*a3)*a4                                            !/*wa         72*/
         an = (a1 + a2 +a3)*a4                                                  !/*wa         73*/
         wamm =af/an                                                            !/*wa         74*/
!         write(io,100) a,z,il,bs,bs0,bs40,bk,a1*a4,a2*a4,a3*a4,a4,             !/*wa         75*/
!     &             af,an,ann,wamm                                              !/*wa         76*/
      endif                                                                     !/*wa         77*/
 100  format(1x,'(A,Z)=',2(f4.0),') ln= ',i4/                                   !/*wa         78*/
     &       'bs,bs0,bs40,bk = ', 1p,4(1x,e10.4),                               !/*wa         79*/
     &       'a1,a2,a3,a4 = ', 4(1x,e10.4)/                                     !/*wa         80*/
     &       'af,an,ann,af/an = ', 3(1x,e10.4),0p,f8.3)                         !/*wa         81*/
#endif                                                                          /*!wa         82*/
      return                                                                    !/*wa         83*/
      end                                                                       !/*wa         84*/
