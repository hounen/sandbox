!#ident brang.F from deck bg                                                    !/*bg          1*/
      subroutine brang                                                          !/*bg          2*/
!        calculate bremsstrahlung angular distributions.                        !/*bg          3*/
      use global_data                                                           !/*bgx         1*/
!                                                                               !/*bgx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*bgx         3*/
      implicit integer(kindi) (i-n)                                             !/*bgx         4*/
!                                                                               !/*bgx         5*/
      dimension ac((mpng+1)/2),cm((mpng+1)/2),ek(mwng),y(mpng),                 !/*bg          5*/
     & xi(mpng),ch(mpng)                                                        !/*bg          6*/
!                                                                               !/*bg          8*/
      ca=0.                                                                     !/*bg          9*/
      n1=(mpng+1)/2                                                             !/*bg4c        1*/
      r2=n1-1                                                                   !/*bg4c        2*/
      do 10 i=1,n1                                                              !/*bg4c        3*/
   10 cm(i)=(i-1)/r2                                                            !/*bg4c        4*/
      do 15 j=jmd(ljmd+mkc),jmd(ljmd+mkc+1)-1                                   !/*bg4c        5*/
      z=iza(liza+j)/1000                                                        !/*bg4c        6*/
   15 ca=ca+(z+1.)*z*fme(lfme+j)                                                !/*bg4c        7*/
!                                                                               !/*bg4c        8*/
      do 150 n=1,nee(3),nstp                                                    !/*bgx         6*/
      s=eee(leee+3,n)/gpt(3)                                                    !/*bgx         7*/
      do 20 k=1,nwng                                                            !/*bg4c       10*/
   20 ek(k)=rka(k)*s                                                            !/*bg4c       11*/
      eo=s+1.                                                                   !/*bg         17*/
      eb=eo**2                                                                  !/*bg         18*/
      p2=s*(s+2.)                                                               !/*bg         19*/
      po=sqrt(p2)                                                               !/*bg         20*/
      b=po/eo                                                                   !/*bg         21*/
      d=eo-po                                                                   !/*bg         22*/
      s2=4.*(2.*eb+1.)/(p2*d**3)                                                !/*bg         23*/
      do 30 l=1,mpng                                                            !/*bg         24*/
      xi(l)=(1.+b*(n1-l)/r2)/(1.+b)                                             !/*bg4c       12*/
   30 ch(l)=(n1-l+r2*b)/(r2+b*(n1-l))                                           !/*bg4c       13*/
      do 150 k=1,nwng                                                           !/*bg4c       14*/
      e=eo-ek(k)                                                                !/*bg         28*/
      ee=eo*e                                                                   !/*bg         29*/
      f2=eb+e**2                                                                !/*bg         30*/
      if(s.gt.4.)go to 50                                                       !/*bg         31*/
      p=sqrt((s-ek(k)+2.)*(s-ek(k)))                                            !/*bg         32*/
      pp=po*p                                                                   !/*bg         33*/
      e2=ek(k)**2                                                               !/*bg         34*/
      ef=p2-ek(k)*eo                                                            !/*bg         35*/
      pf=sqrt(p2*(p2-2.*ek(k)*eo+e2))                                           !/*bg         36*/
      if(ef.le.pf)el=log(b**2*(ef+pf)/(.5*e2*(1.-b**2)))/pp                     !/*bg         37*/
      if(ef.gt.pf)el=log((ef+pf)/(ef-pf))/pp                                    !/*bg         38*/
      ey=log((e+p)/(e-p))                                                       !/*bg         39*/
      f=ca*p/po                                                                 !/*bg         40*/
      f3=(5.*eb+2.*ee+3.)/(d*p2)                                                !/*bg         41*/
      f4=(p2-e2)/d                                                              !/*bg         42*/
      f5=2.*(e/p2-ey/p)                                                         !/*bg         43*/
      f6=2.*eo*(3.*ek(k)-p2*e)/(p2*d**3)                                        !/*bg         44*/
      f7=(2.*eb*f2+1.-(6.*eb-3.*ee+f2))/(d*p2)                                  !/*bg         45*/
      f8=ek(k)*(eb+ee-1.)/p2                                                    !/*bg         46*/
      do 40 l=1,mpng                                                            !/*bg         47*/
      q2=p2+e2-2.*po*ek(k)*ch(l)                                                !/*bg         48*/
      q=sqrt(q2)                                                                !/*bg         49*/
   40 y(l)=f*(s2*(xi(l)**3*(1.-ch(l)**2))-(f3+f4/q2)*xi(l)+f5+                  !/*bg         50*/
     & el*(f6*xi(l)**3*(1.-ch(l)**2)+f7*xi(l)+f8)+(log((q+p)/                   !/*bg         51*/
     & (q-p))/(p*q))*(2.*xi(l)/d-3.*ek(k)-ek(k)*(p2-e2)/q2))/xi(l)              !/*bg         52*/
      go to 130                                                                 !/*bg         53*/
   50 do 60 l=1,mpng                                                            !/*bg         54*/
   60 y(l)=0.                                                                   !/*bg         55*/
      if(s.gt.30.)go to 110                                                     !/*bg         56*/
      do 100 j=jmd(ljmd+mkc),jmd(ljmd+mkc+1)-1                                  !/*bg         57*/
      z=iza(liza+j)/1000                                                        !/*bg         58*/
      if(eb.gt.ek(k)*(eo+20./(3.*z**third)))go to 80                            !/*bg         59*/
      p=sqrt((s-ek(k)+2.)*(s-ek(k)))                                            !/*bg         60*/
      pp=po*p                                                                   !/*bg         61*/
      e2=ek(k)**2                                                               !/*bg         62*/
      ef=p2-ek(k)*eo                                                            !/*bg         63*/
      pf=sqrt(p2*(p2-2.*ek(k)*eo+e2))                                           !/*bg         64*/
      if(ef.le.pf)el=log(b**2*(ef+pf)/(.5*e2*(1.-b**2)))/pp                     !/*bg         65*/
      if(ef.gt.pf)el=log((ef+pf)/(ef-pf))/pp                                    !/*bg         66*/
      ey=log((e+p)/(e-p))                                                       !/*bg         67*/
      f=(z+1.)*z*fme(lfme+j)*p/po                                               !/*bg         68*/
      f3=(5.*eb+2.*ee+3.)/(d*p2)                                                !/*bg         69*/
      f4=(p2-e2)/d                                                              !/*bg         70*/
      f5=2.*(e/p2-ey/p)                                                         !/*bg         71*/
      f6=2.*eo*(3.*ek(k)-p2*e)/(p2*d**3)                                        !/*bg         72*/
      f7=(2.*eb*f2+1.-(6.*eb-3.*ee+f2))/(d*p2)                                  !/*bg         73*/
      f8=ek(k)*(eb+ee-1.)/p2                                                    !/*bg         74*/
      do 70 l=1,mpng                                                            !/*bg         75*/
      q2=p2+e2-2.*po*ek(k)*ch(l)                                                !/*bg         76*/
      q=sqrt(q2)                                                                !/*bg         77*/
   70 y(l)=y(l)+f*(s2*(xi(l)**3*(1.-ch(l)**2))-(f3+f4/q2)*xi(l)+f5+             !/*bg         78*/
     & el*(f6*xi(l)**3*(1.-ch(l)**2)+f7*xi(l)+f8)+(log((q+p)/                   !/*bg         79*/
     & (q-p))/(p*q))*(2.*xi(l)/d-3.*ek(k)-ek(k)*(p2-e2)/q2))/xi(l)              !/*bg         80*/
      go to 100                                                                 !/*bg         81*/
   80 f=4.*(z+1.)*z*fme(lfme+j)*b/eo                                            !/*bg         82*/
      g=log(ek(k)/(2.*ee))+2.                                                   !/*bg         83*/
      a=12.*z**third*ee/(121.*ek(k))                                            !/*bg         84*/
      do 90 l=1,mpng                                                            !/*bg         85*/
      sf=screen(a*xi(l))-g                                                      !/*bg         86*/
   90 y(l)=y(l)+f*(f2*(3.+2.*sf)-2.*ee*(1.+4.*(1.-xi(l))*xi(l)*sf))             !/*bg         87*/
  100 continue                                                                  !/*bg         88*/
      go to 130                                                                 !/*bg         89*/
  110 do 120 j=jmd(ljmd+mkc),jmd(ljmd+mkc+1)-1                                  !/*bg         90*/
      ix=lme(llme+3,j)                                                          !/*bg         91*/
      z=nxs(lnxs+2,ix)                                                          !/*bg         92*/
      f=4.*(z+1.)*z*fme(lfme+j)*b/eo                                            !/*bg         93*/
      g=log(ek(k)/(2.*ee))+2.+exs(jxs(ljxs+8,ix)+2)                             !/*bg         94*/
      a=12.*exs(jxs(ljxs+8,ix))*ee/(121.*ek(k))                                 !/*bg         95*/
      do 120 l=1,mpng                                                           !/*bg         96*/
      sf=screen(a*xi(l))-g                                                      !/*bg         97*/
  120 y(l)=y(l)+f*(f2*(3.+2.*sf)-2.*ee*(1.+4.*(1.-xi(l))*xi(l)*sf))             !/*bg         98*/
  130 ac((mpng+1)/2)=0.                                                         !/*bg         99*/
      do 140 l=3,mpng,2                                                         !/*bg        100*/
      m=(mpng+2-l)/2                                                            !/*bg        101*/
  140 ac(m)=ac(m+1)+y(l-2)+4.*y(l-1)+y(l)                                       !/*bg        102*/
      do 150 l=1,mpng                                                           !/*bg        103*/
  150 ech(lech+l,k,n/nstp+1+(nee(3)/nstp+1)*(mkc-1))=min(max(zero,              !/*bgx         8*/
     & qpol((mpng-l)*(ac(1)/(mpng-1)),ac,cm,(mpng+1)/2)),one)                   !/*bg        105*/
      return                                                                    !/*bg        106*/
      end                                                                       !/*bg        107*/
