!     $Id: aiangl.F,v 1.4 2002/03/27 18:14:52 gwm Exp $                       !/*ai          1*/
      subroutine aiangl (e,u,v,ans,ind13,ind14)                                 !/*ai          2*/
#include <lcs/double.h>                                                         /*!ai          3*/
!     ==================================================================        !/*ai          4*/
!     aiangl is called for isobar capture and scattering.                       !/*ai          5*/
! AIANGL calls   FIND  BINTER  FLTRNF                                           !/*ai          6*/
! called by GAMMA                                                               !/*ai          7*/
!     ==================================================================        !/*ai          8*/
!                                                                               !/*ai          9*/
      dimension ener(40), enec(40), ctaba(21), ctabb(21), ctabc(21),            !/*ai         10*/
     & taba(21), tabb(21), tabc(21)                                             !/*ai         11*/
      data ener /2.176d0,2.297d0,2.317d0,2.337d0,2.357d0,2.377d0,2.397d0        !/*ai         12*/
     & ,2.416d0,2.435d0,2.454d0,2.473d0,2.492d0,2.511d0,2.530d0,2.549d0,        !/*ai         13*/
     & 2.567d0,2.585d0,2.603d0,2.621d0,2.639d0,2.656d0,19*dp0/, enec /2.        !/*ai         14*/
     & 1d0,2.15d0,2.2d0,2.25d0,2.3d0,2.35d0,2.4d0,2.45d0,2.55d0,2.6d0,2.        !/*ai         15*/
     & 65d0,2.7d0,2.75d0,2.8d0,2.95d0,3.1d0,3.3d0,3.45d0,3.6d0,3.95d0,4         !/*ai         16*/
     & .d0,19*dp0/, taba /2.2d0,2.66d0,3.114d0,3.573d0,4.036d0,4.503d0,4        !/*ai         17*/
     & .974d0,5.425d0,5.881d0,6.34d0,6.802d0,7.269d0,7.739d0,8.213d0,8.6        !/*ai         18*/
     & 9d0,9.146d0,9.605d0,10.07d0,10.53d0,11.d0,11.45d0/, tabb /1.37d0,        !/*ai         19*/
     & 1.852d0,2.321d0,2.793d0,3.269d0,3.749d0,4.232d0,4.695d0,5.162d0,5        !/*ai         20*/
     & .632d0,6.105d0,6.582d0,7.063d0,7.547d0,8.034d0,8.499d0,8.967d0,9.        !/*ai         21*/
     & 439d0,9.913d0,10.39d0,10.84d0/, tabc /6.15d0,6.02d0,5.837d0,5.66d        !/*ai         22*/
     & 0,5.49d0,5.327d0,5.169d0,5.024d0,4.885d0,4.75d0,4.619d0,4.493d0,4        !/*ai         23*/
     & .371d0,4.253d0,4.139d0,4.035d0,3.933d0,3.835d0,3.74d0,3.647d0,3.5        !/*ai         24*/
     & 63d0/                                                                    !/*ai         25*/
      data ctaba /-7.47d0,-13.13d0,-8.61d0,-6.67d0,-5.43d0,-4.04d0,-2.52        !/*ai         26*/
     & d0,-1.84d0,-1.31d0,-1.19d0,-1.04d0,-0.95d0,-0.89d0,-0.84d0,-0.73d        !/*ai         27*/
     & 0,-0.67d0,-0.62d0,-0.59d0,-0.56d0,-0.56d0,-0.56d0/, ctabb /14.07d        !/*ai         28*/
     & 0,27.92d0,18.07d0,13.94d0,11.27d0,8.13d0,4.55d0,2.97d0,1.77d0,1.5        !/*ai         29*/
     & d0,1.17d0,0.98d0,0.84d0,0.72d0,0.49d0,0.37d0,0.25d0,0.19d0,0.13d0        !/*ai         30*/
     & ,0.12d0,0.13d0/, ctabc /-6.19d0,-14.47d0,-9.d0,-6.68d0,-5.14d0,-3        !/*ai         31*/
     & .24d0,-1.01d0,0.03d0,0.96d0,1.23d0,1.56d0,1.79d0,2.01d0,2.21d0,2.        !/*ai         32*/
     & 77d0,3.29d0,3.99d0,4.53d0,5.09d0,6.41d0,6.60d0/                          !/*ai         33*/
      ik=0                                                                      !/*ai         34*/
      ie=0                                                                      !/*ai         35*/
      if (ind13-1) 10,170,10                                                    !/*ai         36*/
!     for capture coefficients a and b are fcns of u and v. see notes.          !/*ai         37*/
   10 ik=20                                                                     !/*ai         38*/
      if (u-2.1d0) 20,20,30                                                     !/*ai         39*/
   20 a=ctaba(1)                                                                !/*ai         40*/
      b=ctabb(1)                                                                !/*ai         41*/
      c=ctabc(1)                                                                !/*ai         42*/
      go to 40                                                                  !/*ai         43*/
   30 call find (u,enec,ie,ik)                                                  !/*ai         44*/
      call binter (u,enec,ie,aint3,aint4,aint5)                                 !/*ai         45*/
      a=ctaba(ie-1)*aint3+ctaba(ie)*aint4+ctaba(ie+1)*aint5                     !/*ai         46*/
      b=ctabb(ie-1)*aint3+ctabb(ie)*aint4+ctabb(ie+1)*aint5                     !/*ai         47*/
      c=ctabc(ie-1)*aint3+ctabc(ie)*aint4+ctabc(ie+1)*aint5                     !/*ai         48*/
   40 aa=(a*v*v)+(b*v)+c                                                        !/*ai         49*/
      if (u-2.35d0) 50,60,60                                                    !/*ai         50*/
   50 app=-0.4998d0*u-0.08044d0                                                 !/*ai         51*/
      go to 70                                                                  !/*ai         52*/
   60 app=-1.25d0                                                               !/*ai         53*/
   70 if (u-3.025d0) 80,90,90                                                   !/*ai         54*/
   80 bpp=2.6524d0*u-4.03749d0                                                  !/*ai         55*/
      go to 100                                                                 !/*ai         56*/
   90 bpp=3.52817d0*u-6.72547d0                                                 !/*ai         57*/
  100 bb=app*v+bpp                                                              !/*ai         58*/
!     having gotten aa and bb (equivalent to a and b in notes) calculate        !/*ai         59*/
!     following changes made 8/2/71 to symeterize capture differential          !/*ai         60*/
!     cross sections                                                            !/*ai         61*/
      d1=bb+.01949d0                                                            !/*ai         62*/
      dsq=d1*d1                                                                 !/*ai         63*/
      asq=aa*aa                                                                 !/*ai         64*/
      alpha=bb*dsq                                                              !/*ai         65*/
      beta=asq*(bb-dp2*d1)                                                      !/*ai         66*/
      y=alpha/(dsq*dsq)                                                         !/*ai         67*/
      y1=(alpha+beta)/((dsq-asq)**2)                                            !/*ai         68*/
! calculate maximum y and store in amax.                                        !/*ai         69*/
      if (y-y1) 110,120,120                                                     !/*ai         70*/
  110 y=y1                                                                      !/*ai         71*/
!  also calculate y at where first derivative is zero.  that occurs at          !/*ai         72*/
  120 xsq=dsq*(dp3*bb-dp2*d1)/(asq*(dp2*d1-bb))                                 !/*ai         73*/
      if ((xsq.gt.dp1).or.(xsq.lt.dp0)) go to 130                               !/*ai         74*/
      y1=(alpha+beta*xsq)/((dsq-asq*xsq)**2)                                    !/*ai         75*/
      if (y1-y) 130,140,140                                                     !/*ai         76*/
  130 amax=y                                                                    !/*ai         77*/
      go to 150                                                                 !/*ai         78*/
  140 amax=y1                                                                   !/*ai         79*/
! statement 21 starts rejection method.                                         !/*ai         80*/
  150 continue                                                                  !/*ai         81*/
      rand=rang()                                                               !/*ai         82*/
      ran2=rang()                                                               !/*ai         83*/
      ran1=rand+rand-dp1                                                        !/*ai         84*/
      xsq=ran1*ran1                                                             !/*ai         85*/
      yr1=(alpha+beta*xsq)/((dsq-asq*xsq)**2)                                   !/*ai         86*/
      yr2=ran2*amax                                                             !/*ai         87*/
      if (yr1-yr2) 150,160,160                                                  !/*ai         88*/
  160 ans=ran1                                                                  !/*ai         89*/
      return                                                                    !/*ai         90*/
! statement 100 is for scattering.                                              !/*ai         91*/
  170 ik=20                                                                     !/*ai         92*/
      if (u-2.176d0) 180,180,190                                                !/*ai         93*/
! coefficients are only fcn of u.                                               !/*ai         94*/
  180 a=taba(1)                                                                 !/*ai         95*/
      b=tabb(1)                                                                 !/*ai         96*/
      c=tabc(1)                                                                 !/*ai         97*/
      go to 200                                                                 !/*ai         98*/
  190 call find (u,ener,ie,ik)                                                  !/*ai         99*/
      call binter (u,ener,ie,aint3,aint4,aint5)                                 !/*ai        100*/
      a=taba(ie-1)*aint3+taba(ie)*aint4+taba(ie+1)*aint5                        !/*ai        101*/
      b=tabb(ie-1)*aint3+tabb(ie)*aint4+tabb(ie+1)*aint5                        !/*ai        102*/
      c=tabc(ie-1)*aint3+tabc(ie)*aint4+tabc(ie+1)*aint5                        !/*ai        103*/
! y here is y at 180 degrees.                                                   !/*ai        104*/
  200 d=(-a+b+0.196d0)                                                          !/*ai        105*/
      y=dp1/(d*d+1.d-02*c)                                                      !/*ai        106*/
! ymax here is y at minimum or maximum (at first derivative=0).                 !/*ai        107*/
      ymax=dp1/(1.d-02*c)                                                       !/*ai        108*/
      if (y-ymax) 220,210,210                                                   !/*ai        109*/
  210 ymax=y                                                                    !/*ai        110*/
! y here is y at 0 degrees.                                                     !/*ai        111*/
  220 y=dp1/((a+b+0.196d0)*(a+b+0.196d0)+1.d-02*c)                              !/*ai        112*/
      if (y-ymax) 230,240,240                                                   !/*ai        113*/
! amax = is maximum y.                                                          !/*ai        114*/
  230 amax=ymax                                                                 !/*ai        115*/
      go to 250                                                                 !/*ai        116*/
  240 amax=y                                                                    !/*ai        117*/
! statement 250 starts rejection method.                                        !/*ai        118*/
!                                                                               !/*ai        119*/
  250 continue                                                                  !/*ai        120*/
      ran1=rang()                                                               !/*ai        121*/
      ran2=rang()                                                               !/*ai        122*/
      ran1=ran1+ran1-dp1                                                        !/*ai        123*/
      yr1=dp1/((a*ran1+b+0.196d0)*(a*ran1+b+0.196d0)+1.d-02*c)                  !/*ai        124*/
      yr2=ran2*amax                                                             !/*ai        125*/
      if (yr1-yr2) 250,160,160                                                  !/*ai        126*/
      end                                                                       !/*ai        127*/
