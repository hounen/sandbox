!     $Id: clmb1.F,v 1.5 2002/03/27 18:14:57 gwm Exp $                        !/*cl          1*/
      function clmb1 (ro,eta,ml)                                                !/*cl          2*/
#include <lcs/double.h>                                                         /*!cl          3*/
!                                                                               !/*cl          4*/
!     ETA = C2*Z1*Z2*SQRT(M/E)                                                  !/*cl          5*/
!     RHO = C3*(R1+R2)*SQRT(ME)                                                 !/*cl          6*/
!     M = reduced mass in MeV                                                   !/*cl          7*/
!     E = kinetic energy in C of M                                              !/*cl          8*/
!     C2 = .00516 = fine structure constant / sqrt(2)                           !/*cl          9*/
!     C3 = .007165 = sqrt(2)/(hbar-c)                                           !/*cl         10*/
!                                                                               !/*cl         11*/
      parameter (ln0=81, lt0=21)                                                !/*cl         12*/
      parameter (ln1=61, lt1=61)                                                !/*cl         13*/
      dimension psi0(ln0), trans0(lt0,ln0), x0(lt0), f0(ln0), psi1(ln1),        !/*cl         14*/
     & trans1(lt1,ln1), x1(lt1), f1(ln1)                                        !/*cl         15*/
      data pi /3.14159d0/                                                       !/*cl         16*/
      data c0 /.11225d0/, c1 /dph/, gamma /0.5772157d0/, s3 /0.2020569d0        !/*cl         17*/
     & /, s4 /0.08232323d0/                                                     !/*cl         18*/
      y=dp2*eta                                                                 !/*cl         19*/
      psi=ro*y                                                                  !/*cl         20*/
      if (ro.gt.y) then                                                         !/*cl         21*/
        if (psi.gt.dp4.and.psi.lt.50.d0) then                                   !/*cl         22*/
          prob=clmb2(ro,eta,dumm)                                               !/*cl         23*/
        else                                                                    !/*cl         24*/
          x=exp(log(eta)/6.d0)                                                  !/*cl         25*/
          x3=x*x*x                                                              !/*cl         26*/
          temp=c0+c1*x3                                                         !/*cl         27*/
          temp=temp+ro*x                                                        !/*cl         28*/
          arg=dp1-y*x/temp                                                      !/*cl         29*/
          prob=sqrt(arg)                                                        !/*cl         30*/
        endif                                                                   !/*cl         31*/
        ml=0                                                                    !/*cl         32*/
      else                                                                      !/*cl         33*/
        x=ro/y                                                                  !/*cl         34*/
        if (psi.le.psi0(1)) then                                                !/*cl         35*/
          t=min(pi*y,dp6906m2)                                                  !/*cl         36*/
          cx=t/(exp(t)-dp1)                                                     !/*cl         37*/
          t1=cos(ro)*(dp1-.75d0*psi**2+dp5*x*psi**2)-dph*psi*ro*sin(ro)         !/*cl         38*/
          t2=dp1+dph*psi*(dp1-x/6.d0)                                           !/*cl         39*/
          if (eta.gt.dp1) then                                                  !/*cl         40*/
            t3=log(psi)+dp2*gamma-dp1+dp1/(12.d0*eta**2)+dp1/(12.d1*eta*        !/*cl         41*/
     &      *4)                                                                 !/*cl         42*/
          else                                                                  !/*cl         43*/
            t3=log(dp2*ro)+gamma-dp1/(dp1+eta**2)+s3*eta**2+s4*eta**4           !/*cl         44*/
          endif                                                                 !/*cl         45*/
          g=t1+psi*t2*t3                                                        !/*cl         46*/
          f=cx*ro*t2                                                            !/*cl         47*/
          prob=cx/(g**2+f**2)                                                   !/*cl         48*/
          ml=3                                                                  !/*cl         49*/
        elseif (psi.le.psi0(ln0)) then                                          !/*cl         50*/
          if (x.le.x0(1)) then                                                  !/*cl         51*/
            temp=log(psi/psi0(1))                                               !/*cl         52*/
            j0=1+int(temp/delp0)                                                !/*cl         53*/
            j0=min(max(j0,1),ln0-1)                                             !/*cl         54*/
            temp=temp-(j0-1)*delp0                                              !/*cl         55*/
            t=f0(j0)+(f0(j0+1)-f0(j0))*temp/delp0                               !/*cl         56*/
            xk=x*sqrt(psi)                                                      !/*cl         57*/
            prob=(dp1+3.33d-1*x+3.d-1*xk+1.d-1*xk**2)*exp(t)                    !/*cl         58*/
            t=min(pi*y,dp6906m2)                                                !/*cl         59*/
            cx=t/(exp(t)-dp1)                                                   !/*cl         60*/
            prob=cx/prob**2                                                     !/*cl         61*/
            ml=1                                                                !/*cl         62*/
          else                                                                  !/*cl         63*/
            temp1=log(x/x0(1))                                                  !/*cl         64*/
            i0=1+int(temp1/delx0)                                               !/*cl         65*/
            i0=min(max(i0,1),lt0-1)                                             !/*cl         66*/
            temp1=temp1-(i0-1)*delx0                                            !/*cl         67*/
            temp2=log(psi/psi0(1))                                              !/*cl         68*/
            j0=1+int(temp2/delp0)                                               !/*cl         69*/
            j0=min(max(j0,1),ln0-1)                                             !/*cl         70*/
            temp2=temp2-(j0-1)*delp0                                            !/*cl         71*/
            t1=trans0(i0,j0)+(trans0(i0+1,j0)-trans0(i0,j0))*temp1/delx0        !/*cl         72*/
            t2=trans0(i0,j0+1)+(trans0(i0+1,j0+1)-trans0(i0,j0+1))*temp1        !/*cl         73*/
     &      /delx0                                                              !/*cl         74*/
            t=t1+(t2-t1)*temp2/delp0                                            !/*cl         75*/
            ml=2                                                                !/*cl         76*/
            prob=exp(t)                                                         !/*cl         77*/
          endif                                                                 !/*cl         78*/
        elseif (psi.le.psi1(ln1)) then                                          !/*cl         79*/
          if (x.le.x1(1)) then                                                  !/*cl         80*/
            temp=log(psi/psi1(1))                                               !/*cl         81*/
            j0=1+int(temp/delp1)                                                !/*cl         82*/
            j0=min(max(j0,1),ln1-1)                                             !/*cl         83*/
            temp=temp-(j0-1)*delp1                                              !/*cl         84*/
            t=f1(j0)+(f1(j0+1)-f1(j0))*temp/delp1                               !/*cl         85*/
            xk=x*sqrt(psi)                                                      !/*cl         86*/
            prob=(dp1+3.33d-1*x+3.d-1*xk+1.d-1*xk**2)*exp(t)                    !/*cl         87*/
            t=min(pi*y,dp6906m2)                                                !/*cl         88*/
            cx=t/(exp(t)-dp1)                                                   !/*cl         89*/
            prob=cx/prob**2                                                     !/*cl         90*/
            ml=1                                                                !/*cl         91*/
          else                                                                  !/*cl         92*/
            temp1=log(x/x1(1))                                                  !/*cl         93*/
            i0=1+int(temp1/delx1)                                               !/*cl         94*/
            i0=min(max(i0,1),lt1-1)                                             !/*cl         95*/
            temp1=temp1-(i0-1)*delx1                                            !/*cl         96*/
            temp2=log(psi/psi1(1))                                              !/*cl         97*/
            j0=1+int(temp2/delp1)                                               !/*cl         98*/
            j0=min(max(j0,1),ln1-1)                                             !/*cl         99*/
            temp2=temp2-(j0-1)*delp1                                            !/*cl        100*/
            t1=trans1(i0,j0)+(trans1(i0+1,j0)-trans1(i0,j0))*temp1/delx1        !/*cl        101*/
            t2=trans1(i0,j0+1)+(trans1(i0+1,j0+1)-trans1(i0,j0+1))*temp1        !/*cl        102*/
     &      /delx1                                                              !/*cl        103*/
            t=t1+(t2-t1)*temp2/delp1                                            !/*cl        104*/
            ml=2                                                                !/*cl        105*/
            prob=exp(t)                                                         !/*cl        106*/
          endif                                                                 !/*cl        107*/
        else                                                                    !/*cl        108*/
          prob=clmb2(ro,eta,dumm)                                               !/*cl        109*/
          ml=4                                                                  !/*cl        110*/
        endif                                                                   !/*cl        111*/
      endif                                                                     !/*cl        112*/
      clmb1=prob                                                                !/*cl        113*/
      return                                                                    !/*cl        114*/
      entry rdtab(lun)                                                          !/*cl        115*/
      read (lun) delp0,delx0                                                    !/*cl        116*/
      read (lun) psi0                                                           !/*cl        117*/
      read (lun) x0                                                             !/*cl        118*/
      read (lun) f0                                                             !/*cl        119*/
      read (lun) trans0                                                         !/*cl        120*/
      read (lun) delp1,delx1                                                    !/*cl        121*/
      read (lun) psi1                                                           !/*cl        122*/
      read (lun) x1                                                             !/*cl        123*/
      read (lun) f1                                                             !/*cl        124*/
      read (lun) trans1                                                         !/*cl        125*/
      clmb1=dp0                                                                 !/*cl        126*/
      return                                                                    !/*cl        127*/
      end                                                                       !/*cl        128*/
