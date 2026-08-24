!     $Id: glando.F,v 1.7 2002/03/27 18:14:49 gwm Exp $                       !/*gl          1*/
      subroutine glando(ran_num,step,zp,z,a,rho,t,xmass,de,iflag)               !/*gl          2*/
!                                                                               !/*gl          3*/
!     ******************************************************************        !/*gl          4*/
!     *                                                                *        !/*gl          5*/
!     *  glando                                                        *        !/*gl          6*/
!     *  ------                                                        *        !/*gl          7*/
!     *  energy straggling using gaussian, landau & vavilov theories.  *        !/*gl          8*/
!     *                                                                *        !/*gl          9*/
!     *  input                                                         *        !/*gl         10*/
!     *  -----                                                         *        !/*gl         11*/
!     *  ran_num() is a random number generator, returning the next    *        !/*gl         12*/
!     *            in a sequence, with 0. < ran_num() < 1.             *        !/*gl         13*/
!     *                                                                *        !/*gl         14*/
!     *  step  =  current step-length (cm)                             *        !/*gl         15*/
!     *                                                                *        !/*gl         16*/
!     *  zp    = atomic number ( +/- charge ) of projectile particle   *        !/*gl         17*/
!     *                                                                *        !/*gl         18*/
!     *  z     = atomic number of medium                               *        !/*gl         19*/
!     *                                                                *        !/*gl         20*/
!     *  a     = atomic weight of medium                               *        !/*gl         21*/
!     *                                                                *        !/*gl         22*/
!     *  rho   = mass density of medium (gm/cm**3)                     *        !/*gl         23*/
!     *                                                                *        !/*gl         24*/
!     *  t     = kinetic energy (MeV)                                  *        !/*gl         25*/
!     *                                                                *        !/*gl         26*/
!     *  xmass = projectile mass (MeV)                                 *        !/*gl         27*/
!     *                                                                *        !/*gl         28*/
!     *  output                                                        *        !/*gl         29*/
!     *  ------                                                        *        !/*gl         30*/
!     *  de     =  ( de/dx - <de/dx> ) * step   (MeV)                  *        !/*gl         31*/
!     *                                                                *        !/*gl         32*/
!     *  iflag  =  0   step too small, CSDA assumed                    *        !/*gl         33*/
!     *         =  4   landau sampling used (cappa < 0.01)             *        !/*gl         34*/
!     *         =  5   vavilov sampling used (0.01 <= cappa <= 10.0)   *        !/*gl         35*/
!     *         =  6   gaussian sampling used (cappa > 10.0)           *        !/*gl         36*/
!     *                                                                *        !/*gl         37*/
!     *  warning (as stated, but unconfirmed locally)                  *        !/*gl         38*/
!     *  --------------------------------------------                  *        !/*gl         39*/
!     *  only landau sampling should be used since this has been well  *        !/*gl         40*/
!     *  tested whereas both vavilov and gaussian sampling are being   *        !/*gl         41*/
!     *  developed.                                                    *        !/*gl         42*/
!     *                                                                *        !/*gl         43*/
!     *  author      : g.n. patrick                                    *        !/*gl         44*/
!     *  date        : 03.05.1985                                      *        !/*gl         45*/
!     *  last update : 09.09.1985                                      *        !/*gl         46*/
!     *                                                                *        !/*gl         47*/
!     ******************************************************************        !/*gl         48*/
!                                                                               !/*gl         49*/
#include <gvaviv/double.h>                                                      /*!gl         50*/
      external ran_num                                                          !/*gl         51*/
      parameter (euler=.5772156649015328606065120900824d0)                      !/*gl         52*/
      parameter (pi=3.14159265358979323846264338328d0)                          !/*gl         53*/
      parameter (p1=.60715d+0,p2=.67794d+0,p3=.52382d-1,p4=.94753d+0,           !/*gl         54*/
     &           p5=.74442d+0,p6=1.1934d+0)                                     !/*gl         55*/
!                                                                               !/*gl         56*/
      parameter (emass=0.5109990615d+0)                                         !/*gl         57*/
!                                                                               !/*gl         58*/
      fland(x) = p1+p6*x+(p2+p3*x)*exp(p4*x+p5)                                 !/*gl         59*/
!                                                                               !/*gl         60*/
!     ------------------------------------------------------------------        !/*gl         61*/
!                                                                               !/*gl         62*/
      if(step.lt.0.0000001)then                                                 !/*gl         63*/
          de=0.                                                                 !/*gl         64*/
          iflag = 0                                                             !/*gl         65*/
          return                                                                !/*gl         66*/
      endif                                                                     !/*gl         67*/
!                                                                               !/*gl         68*/
!     calculate xi factor (kev).                                                !/*gl         69*/
!                                                                               !/*gl         70*/
      s     = t/xmass                                                           !/*gl         71*/
      eta   = sqrt(s*(s+2.))                                                    !/*gl         72*/
      gamma = s + 1.                                                            !/*gl         73*/
      beta  = eta/gamma                                                         !/*gl         74*/
      xi    = (153.5*zp*zp*z*step*rho)/(a*beta*beta)                            !/*gl         75*/
!                                                                               !/*gl         76*/
!     maximum energy transfer to atomic electron (kev).                         !/*gl         77*/
!                                                                               !/*gl         78*/
      etasq = eta*eta                                                           !/*gl         79*/
      ratio = emass/xmass                                                       !/*gl         80*/
      f1    = 2.*emass*etasq                                                    !/*gl         81*/
      f2    = 1.+2.*ratio*gamma+ratio*ratio                                     !/*gl         82*/
      emax  = 1000.*f1/f2                                                       !/*gl         83*/
!                                                                               !/*gl         84*/
!     calculate kappa significance ratio.                                       !/*gl         85*/
!                                                                               !/*gl         86*/
      cappa  = xi/emax                                                          !/*gl         87*/
!                                                                               !/*gl         88*/
!     choose correct straggling function.                                       !/*gl         89*/
!                                                                               !/*gl         90*/
      if (cappa.lt.0.01) then                                                   !/*gl         91*/
!     sample lambda variable from james & hancock landau distribution           !/*gl         92*/
          iflag = 4                                                             !/*gl         93*/
          xmean = -beta**2-log(xi/emax)+euler-1.                                !/*gl         94*/
          xlamx = fland(xmean)                                                  !/*gl         95*/
  25      call glandg(ran_num,xlamb)                                            !/*gl         96*/
          if(xlamb.gt.xlamx) go to 25                                           !/*gl         97*/
!                                                                               !/*gl         98*/
      else if (cappa.le.10.) then                                               !/*gl         99*/
!     sample lambda variable (landau not vavilov) from                          !/*gl        100*/
!     rotondi & montagna & kolbig vavilov distribution                          !/*gl        101*/
          iflag = 5                                                             !/*gl        102*/
          rka = cappa                                                           !/*gl        103*/
          be2 = beta*beta                                                       !/*gl        104*/
          ra = ran_num()                                                        !/*gl        105*/
          xlamb = gvaviv(rka,be2,ra)                                            !/*gl        106*/
!                                                                               !/*gl        107*/
      else                                                                      !/*gl        108*/
!     sample from gaussian distribution                                         !/*gl        109*/
          iflag = 6                                                             !/*gl        110*/
          sigma  = xi*emax*(1.-(beta*beta/2.))                                  !/*gl        111*/
          sigma  = sqrt(sigma)                                                  !/*gl        112*/
   30     r1 = ran_num()                                                        !/*gl        113*/
          r2 = ran_num()                                                        !/*gl        114*/
          if(r1.le.0.)go to 30                                                  !/*gl        115*/
          f1     = -2.*log(r1)                                                  !/*gl        116*/
          dekev  = sigma*sqrt(f1)*cos(2.*pi*r2)                                 !/*gl        117*/
          goto 40                                                               !/*gl        118*/
      endif                                                                     !/*gl        119*/
!                                                                               !/*gl        120*/
!     calculate ( de/dx - <de/dx> ) * step                                      !/*gl        121*/
!                                                                               !/*gl        122*/
      dekev  = xi*(xlamb+beta*beta+log(xi/emax)-euler+1.)                       !/*gl        123*/
!                                                                               !/*gl        124*/
   40 de=0.001*dekev                                                            !/*gl        125*/
      return                                                                    !/*gl        126*/
      end                                                                       !/*gl        127*/
