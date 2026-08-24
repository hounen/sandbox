!#ident ssmsrc.F from deck ms                                                   !/*ms          1*/
      subroutine ssmsrc(po,jf)                                                  !/*ms          2*/
!        sample from a spherically-symmetric surface source.                    !/*ms          3*/
!        po = polar angle of the track.                                         !/*ms          4*/
!        return with jf=1 if cylindrical-window sampling fails.                 !/*ms          5*/
      use global_data                                                           !/*msx         1*/
!                                                                               !/*msx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*msx         3*/
      implicit integer(kindi) (i-n)                                             !/*msx         4*/
!                                                                               !/*msx         5*/
      dimension a(2),bt(2),d(2),r(2)                                            !/*ms          7*/
!                                                                               !/*ms          8*/
!        sample the position on the sphere.                                     !/*ms          9*/
      jf=0                                                                      !/*ms         10*/
      if(idefv(10).ne.0)go to 30                                                !/*ms         11*/
      if(idefv(11).ne.0)go to 10                                                !/*ms         12*/
      call isos(tpp,0)                                                          !/*ms4b        1*/
      go to 20                                                                  !/*ms         14*/
   10 a(1)=srv(1,11)                                                            !/*ms4c        1*/
      if(ivdis(11).ne.0)call smpsrc(a,ivdis(11),ib,fi)                          !/*ms4c        2*/
      call rotas(a(1),srv(1,12),tpp,0,irt)                                      !/*ms4c        3*/
   20 xxx=rssp*tpp(1)                                                           !/*ms         18*/
      yyy=rssp*tpp(2)                                                           !/*ms         19*/
      zzz=rssp*tpp(3)                                                           !/*ms         20*/
!                                                                               !/*ms         21*/
!        sample the azimuthal angle.                                            !/*ms         22*/
      call rotas(po,tpp,uuu,0,irt)                                              !/*ms4b        3*/
      return                                                                    !/*ms         24*/
!                                                                               !/*ms         25*/
!        bias through a cylindrical window.                                     !/*ms         26*/
!        calculate inner and outer angular bounds of emission area              !/*ms         27*/
!        and weight reduction factor for area.                                  !/*ms         28*/
   30 ph=acos(po)                                                               !/*ms         29*/
      sp=sqrt(1.-po**2)                                                         !/*ms         30*/
      p1=0.                                                                     !/*ms         31*/
      e=asin(sp*rssp/bcw(1,1))                                                  !/*ms         32*/
      if(ph.le.bcw(1,3))go to 40                                                !/*ms         33*/
      p1=ph-bcw(1,2)-e                                                          !/*ms         34*/
      if(ph.le.bcw(2,3))go to 40                                                !/*ms         35*/
      p2=ph+bcw(2,2)-asin(sp*rssp/bcw(2,1))                                     !/*ms         36*/
      go to 50                                                                  !/*ms         37*/
   40 p2=ph+bcw(1,2)-e                                                          !/*ms         38*/
   50 dc=cos(p1)-cos(p2)                                                        !/*ms         39*/
      if(dc.gt.0.)go to 60                                                      !/*ms         40*/
      jf=1                                                                      !/*ms         41*/
      return                                                                    !/*ms         42*/
!                                                                               !/*ms         43*/
!        sample emission position in field of view.                             !/*ms         44*/
   60 ni=1                                                                      !/*ms         45*/
   70 cv=cos(p1)-rang()*dc                                                      !/*ms         46*/
      pv=acos(cv)                                                               !/*ms         47*/
      sv=sin(pv)                                                                !/*ms         48*/
      th=rang()*2.*pie                                                          !/*ms         49*/
      ct=cos(th)                                                                !/*ms         50*/
      st=sin(th)                                                                !/*ms         51*/
      xxx=rssp*sv*ct                                                            !/*ms         52*/
      yyy=rssp*sv*st                                                            !/*ms         53*/
      zzz=rssp*cv                                                               !/*ms         54*/
      if(abs(po).lt.1.)go to 80                                                 !/*ms         55*/
      uuu=sv*ct                                                                 !/*ms         56*/
      vvv=sv*st                                                                 !/*ms         57*/
      www=cv                                                                    !/*ms         58*/
      wc=1.                                                                     !/*ms         59*/
      go to 120                                                                 !/*ms         60*/
!                                                                               !/*ms         61*/
!        calculate arc limits and sample position on arc.                       !/*ms         62*/
   80 a(2)=pie                                                                  !/*ms         63*/
      k=0                                                                       !/*ms         64*/
   90 k=k+1                                                                     !/*ms         65*/
      bt(k)=ph-asin(sp*rssp/bcw(k,1))                                           !/*ms         66*/
      d(k)=bcw(k,1)*cos(bt(k))                                                  !/*ms         67*/
      r(k)=bcw(k,1)*sin(bt(k))                                                  !/*ms         68*/
      if(bt(1)+pv.le.bcw(1,2))go to 100                                         !/*ms         69*/
      s=(srv(4-k,10)-d(k)*cv)/(r(k)*sv)                                         !/*ms         70*/
      a(k)=0.                                                                   !/*ms         71*/
      if(s.lt.1.)a(k)=acos(s)                                                   !/*ms         72*/
      if(pv.gt.bcw(2,3).and.k.eq.1)go to 90                                     !/*ms         73*/
      if(a(1).le.a(2))k=1                                                       !/*ms         74*/
      ah=a(k)                                                                   !/*ms         75*/
      wc=ah/pie                                                                 !/*ms         76*/
      if(wc.ne.0.)go to 110                                                     !/*ms         77*/
      ni=ni+1                                                                   !/*ms         78*/
      if(ni.lt.11)go to 70                                                      !/*ms         79*/
      jf=1                                                                      !/*ms         80*/
      return                                                                    !/*ms         81*/
  100 ah=pie                                                                    !/*ms         82*/
      k=1                                                                       !/*ms         83*/
      wc=1.                                                                     !/*ms         84*/
  110 et=(2.*rang()-1.)*ah                                                      !/*ms         85*/
!                                                                               !/*ms         86*/
!        calculate direction cosines and weight.                                !/*ms         87*/
      dx=r(k)*cos(et)                                                           !/*ms         88*/
      rx=r(k)*sin(et)                                                           !/*ms         89*/
      s=d(k)*sv-dx*cv                                                           !/*ms         90*/
      uuu=s*ct-rx*st-xxx                                                        !/*ms         91*/
      vvv=s*st+rx*ct-yyy                                                        !/*ms         92*/
      www=d(k)*cv+dx*sv-zzz                                                     !/*ms         93*/
      s=1./sqrt(uuu**2+vvv**2+www**2)                                           !/*ms         94*/
      uuu=uuu*s                                                                 !/*ms         95*/
      vvv=vvv*s                                                                 !/*ms         96*/
      www=www*s                                                                 !/*ms         97*/
  120 wgt=.5*dc*wc*wgt                                                          !/*ms         98*/
      return                                                                    !/*ms         99*/
      end                                                                       !/*ms        100*/
