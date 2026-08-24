!     $Id: xabs.F,v 1.4 2002/03/27 18:14:50 gwm Exp $                         !/*xa          1*/
      function xabs (zp,ap,zt,at,ep)	                                           !/*xa          2*/
!  NASA subroutine                                                              !/*xa          3*/
#include <lcs/double.h>                                                         /*!xa          4*/
! absoprption xsec revised version rkt-97/5                                     !/*xa          5*/
! neutron data from barashenkov                                                 !/*xa          6*/
! this gives absorption xsec for given zp,ap,zt,at,e (mev/nucleon)              !/*xa          7*/
! arguement changed to mev; then e=ep/ap mev/nucleon                            !/*xa          8*/
! can be used for neutrons also.                                                !/*xa          9*/
!     this has coulomb as ours                                                  !/*xa         10*/
      e=ep/ap                                                                   !/*xa         11*/
      xq1=zp                                                                    !/*xa         12*/
      xq2=ap	                                                                   !/*xa         13*/
      xq3=zt                                                                    !/*xa         14*/
      xq4=at	                                                                   !/*xa         15*/
!                                                                               !/*xa         16*/
! nucleon-nucleon inelastc xsec not included here                               !/*xa         17*/
!                                                                               !/*xa         18*/
      if (nint(ap*at).eq.1.or.nint(zp+zt).eq.1) then                            !/*xa         19*/
!     if (nint(zp*zt).eq.1.or.nint(zp+zt).eq.1) then                            !/*xa         20*/
        xabs=dp0                                                                !/*xa         21*/
        return                                                                  !/*xa         22*/
      endif                                                                     !/*xa         23*/
      rp=radius(ap)                                                             !/*xa         24*/
      rt=radius(at)                                                             !/*xa         25*/
      vp=(dp1+dpth)*dppi*rp**3                                                  !/*xa         26*/
      vt=(dp1+dpth)*dppi*rt**3                                                  !/*xa         27*/
      density=dph*((ap/vp)+(at/vt))                                             !/*xa         28*/
      const=1.75d0*density/8.824728d-02                                         !/*xa         29*/
!     if (zt.lt.zp) then                                                        !/*xa         30*/
      if (zt.lt.zp.or.(zt.eq.zp.and.at.lt.ap)) then                             !/*xa         31*/
        xzt=zp                                                                  !/*xa         32*/
        xat=ap                                                                  !/*xa         33*/
        zp=zt                                                                   !/*xa         34*/
        ap=at                                                                   !/*xa         35*/
        zt=xzt                                                                  !/*xa         36*/
        at=xat                                                                  !/*xa         37*/
      endif                                                                     !/*xa         38*/
      if (nint(ap).eq.1) const=2.05d0                                           !/*xa         39*/
      if (nint(zp).eq.2.and.nint(ap).eq.4) then                                 !/*xa         40*/
        const1=2.77-at*8.0d-03+(at*at)*1.8d-05                                  !/*xa         41*/
      endif                                                                     !/*xa         42*/
      if (nint(zp).eq.3) const=const/3.                                         !/*xa         43*/
      t1=40.d0                                                                  !/*xa         44*/
      if (nint(zp).eq.0) then                                                   !/*xa         45*/
        if (nint(at).ge.11.and.nint(at).lt.40) t1=30.d0                         !/*xa         46*/
        if (nint(zt).eq.14) t1=35.                                              !/*xa         47*/
        if (nint(zt).eq.26) t1=30.                                              !/*xa         48*/
      endif                                                                     !/*xa         49*/
      gcm=(ap*(dp1+e/938.d0)+at)/(ap**2+at**2+dp2*ap*(e+938.d0)*at/938          !/*xa         50*/
     & .d0)**dph                                                                !/*xa         51*/
      bcm=sqrt(dp1-dp1/gcm**2)                                                  !/*xa         52*/
      plab=ap*sqrt(dp2*938.d0*e+e*e)                                            !/*xa         53*/
      ecmp=gcm*(e+938.d0)*ap-bcm*gcm*plab-ap*938.d0                             !/*xa         54*/
      ecmt=gcm*938.d0*at-at*938.d0                                              !/*xa         55*/
      rela=ecmp+ecmt                                                            !/*xa         56*/
      ecm=rela                                                                  !/*xa         57*/
      if (ecm.lt.0.1d0*rela) ecm=0.1d0*rela                                     !/*xa         58*/
      rm=(197.32/137.01)*zp*zt/ecm                                              !/*xa         59*/
      bigr=rp+rt+1.2*(ap**dpth+at**dpth)/(ecm**dpth)                            !/*xa         60*/
      bigb=1.44*zp*zt/bigr                                                      !/*xa         61*/
      if (nint(zp).eq.1.and.nint(at).gt.56) bigb=0.90*bigb                      !/*xa         62*/
      if (nint(ap).gt.56.and.nint(zt).eq.1) bigb=0.90*bigb                      !/*xa         63*/
      if (nint(ap).eq.1.and.nint(at).eq.12) bigb=3.5*bigb                       !/*xa         64*/
      if (nint(ap).eq.1) then                                                   !/*xa         65*/
        if (nint(at).le.16.and.nint(at).ge.13) then                             !/*xa         66*/
          bigb=(at/7.)*bigb                                                     !/*xa         67*/
        endif                                                                   !/*xa         68*/
        if (nint(zt).eq.12) bigb=1.8*bigb                                       !/*xa         69*/
        if (nint(zt).eq.14) bigb=1.4*bigb                                       !/*xa         70*/
        if (nint(zt).eq.20) bigb=1.3*bigb                                       !/*xa         71*/
      endif                                                                     !/*xa         72*/
      if (nint(ap).eq.1.and.nint(at).lt.4) bigb=21.0*bigb                       !/*xa         73*/
      if (nint(ap).lt.4.and.nint(at).eq.1) bigb=21.0*bigb                       !/*xa         74*/
      if (nint(ap).eq.1.and.nint(at).eq.4) bigb=27.0*bigb                       !/*xa         75*/
      if (nint(ap).eq.4.and.nint(at).eq.1) bigb=27.0*bigb                       !/*xa         76*/
      if (nint(zp).eq.0.or.nint(zt).eq.0) bigb=dp0	                             !/*xa         77*/
      xsec=dp10*dppi*bigr*bigr*(dp1-bigb/ecm)			                                !/*xa         78*/
      xm=1.                                                                     !/*xa         79*/
      if (nint(zp).eq.0) then                                                   !/*xa         80*/
        if (nint(at).lt.200) then                                               !/*xa         81*/
          x1=2.83-3.1d-02*at+1.7d-04*at*at                                      !/*xa         82*/
          if (x1.le.1) x1=1.	                                                   !/*xa         83*/
          sl=dp1                                                                !/*xa         84*/
          if (nint(at).eq.12) sl=1.6                                            !/*xa         85*/
          if (nint(at).lt.12) sl=0.6                                            !/*xa         86*/
          xm=(1-x1*texp(-e/(sl*x1)))                                            !/*xa         87*/
          if (e.lt.20) print *, 'e,xm=',e,xm                                    !/*xa         88*/
        else                                                                    !/*xa         89*/
          xm=(1-0.3*texp(-(e-1)/15))*(1-texp(-(e-0.9))) !7/11/96                !/*xa         90*/
        endif                                                                   !/*xa         91*/
      endif                                                                     !/*xa         92*/
      if (nint(zp).eq.2.and.nint(ap).eq.4) then                                 !/*xa         93*/
        const=const1-0.8/(1+texp((250.d0-e)/75.d0))                             !/*xa         94*/
      endif                                                                     !/*xa         95*/
      if (nint(zp).eq.1.and.nint(ap).eq.1) then                                 !/*xa         96*/
        if (nint(at).gt.45) then                                                !/*xa         97*/
          t1=40.d0+at/dp3                                                       !/*xa         98*/
        endif                                                                   !/*xa         99*/
        if (nint(at).lt.4) then                                                 !/*xa        100*/
          t1=55                                                                 !/*xa        101*/
        endif                                                                   !/*xa        102*/
        const=2.05d0-0.05/(1+texp((250.d0-e)/75.d0))                            !/*xa        103*/
        if (nint(at).lt.4) const=1.7                                            !/*xa        104*/
        if (nint(zt).eq.12) then                                                !/*xa        105*/
          t1=40.d0                                                              !/*xa        106*/
          const=2.05d0-dp3/(1.+texp((e-20.d0)/dp10))                            !/*xa        107*/
        endif                                                                   !/*xa        108*/
        if (nint(zt).eq.14) then                                                !/*xa        109*/
          t1=40.d0                                                              !/*xa        110*/
          const=2.05d0-1.75d0/(1.+texp((e-20.d0)/dp10))                         !/*xa        111*/
        endif                                                                   !/*xa        112*/
        if (nint(zt).eq.18) then                                                !/*xa        113*/
          t1=40.d0                                                              !/*xa        114*/
          const=2.05d0-dp2/(1.+texp((e-20.d0)/dp10))                            !/*xa        115*/
        endif                                                                   !/*xa        116*/
        if (nint(zt).eq.20) then                                                !/*xa        117*/
          t1=40.d0                                                              !/*xa        118*/
          const=2.05d0-dp1/(1.+texp((e-40.d0)/dp10))                            !/*xa        119*/
          const=const-0.25d0/(1+texp((250.d0-e)/75.d0))                         !/*xa        120*/
        endif                                                                   !/*xa        121*/
        if (nint(zt).ge.35) then                                                !/*xa        122*/
          phst=(nint(zt)-35.d0)/260.d0	                                         !/*xa        123*/
          const=const-phst/(1+texp((250.d0-e)/75.d0))                           !/*xa        124*/
        endif                                                                   !/*xa        125*/
      endif                                                                     !/*xa        126*/
      if (nint(zp).eq.0.and.nint(ap).eq.1) then                                 !/*xa        127*/
        const=2*(0.134457/density)                                              !/*xa        128*/
        if (nint(at).gt.140.and.nint(at).lt.200) const=const-1.5d0*(at-         !/*xa        129*/
     & dp2*zt)/at                                                               !/*xa        130*/
        if (nint(at).lt.60) const=const-1.5d0*(at-dp2*zt)/at                    !/*xa        131*/
        if (nint(at).le.40) const=const+0.25d0/(dp1+texp(-(170.d0-e)/           !/*xa        132*/
     & 100.d0))                                                                 !/*xa        133*/
        if (nint(zt).gt.82) const=const-zt/(at-zt)                              !/*xa        134*/
        if (nint(zt).ge.82) const=const-dp2/(1.+texp((e-20.d0)/20.d0))          !/*xa        135*/
        if (nint(zt).le.20.and.nint(zt).ge.10) const=const-dp1/(dp1+texp        !/*xa        136*/
     & ((e-20.d0)/dp10))                                                        !/*xa        137*/
      endif                                                                     !/*xa        138*/
      ce=const*(1.-texp(-e/t1))-0.292*texp(-e/792)*cos(0.229*e**0.453)          !/*xa        139*/
      term1=(at*ap)**dpth/(at**dpth+ap**dpth)                                   !/*xa        140*/
      delta=1.615*term1-0.873d0*ce                                              !/*xa        141*/
      delta=delta+0.140*term1/ecm**dpth                                         !/*xa        142*/
      delta=delta+0.794d0*(at-dp2*zt)*zp/(at*ap)                                !/*xa        143*/
      delta=-delta                                                              !/*xa        144*/
      beta=1.                                                                   !/*xa        145*/
      twxsec=dp10*dppi*1.26d0*1.26d0*beta*(0.873d0*ap**dpth+0.873d0*at*         !/*xa        146*/
     & *dpth-delta)**2                                                          !/*xa        147*/
      xabs=twxsec*(dp1-bigb/ecm)*xm                                             !/*xa        148*/
      if (xabs.lt.dp0) xabs=dp0                                                 !/*xa        149*/
      zp=xq1                                                                    !/*xa        150*/
      ap=xq2                                                                    !/*xa        151*/
      zt=xq3                                                                    !/*xa        152*/
      at=xq4                                                                    !/*xa        153*/
      return                                                                    !/*xa        154*/
      end                                                                       !/*xa        155*/
