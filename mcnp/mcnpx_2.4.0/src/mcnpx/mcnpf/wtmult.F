!#ident wtmult.F from deck wt                                                   !/*wt          1*/
      subroutine wtmult(t,kk)                                                   !/*wt          2*/
!        multiply the tally weight t by the energy-dependent multiplier         !/*wt          3*/
!        for the multiplier bin at kk.                                          !/*wt          4*/
!        tds(ip-1) = index of the material                                      !/*wt          5*/
!        tds(ip) = number of mt reactions (negative if elastic or total)        !/*wt          6*/
!        tds(ip+1,2,...) = mt reaction numbers                                  !/*wt          7*/
      use global_data                                                           !/*wtx         1*/
!                                                                               !/*wtx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*wtx         3*/
      implicit integer(kindi) (i-n)                                             !/*wtx         4*/
!                                                                               !/*wtx         5*/
!                                                                               !/*wt          9*/
      jf=0                                                                      !/*wt4b        1*/
   10 ip=tds(iptal(lipt+5,1,ital)+kk)                                           !/*wt         10*/
      if(ip.ge.-2)go to 20                                                      !/*wt4b        2*/
      kk=kk+1                                                                   !/*wt         12*/
      go to 10                                                                  !/*wt         13*/
   20 if(ip.eq.0)return                                                         !/*wt         14*/
      if(ip.lt.0)go to 280                                                      !/*wt         15*/
      s1=totm                                                                   !/*wt         16*/
      t1=ttn                                                                    !/*wt         17*/
      if(tds(ip-1).lt.0.)go to 220                                              !/*wt         18*/
      h=0.                                                                      !/*wt         19*/
      mk=tds(ip-1)                                                              !/*wt         20*/
      if(mk.eq.0)go to 200                                                      !/*wt         21*/
      if(ipt.eq.2)go to 140                                                     !/*wt         22*/
      if(ipt.eq.9)go to 192                                                     !/*wtxl        1*/
!                                                                               !/*wt         23*/
!        neutron multipliers                                                    !/*wt         24*/
      if(tds(ip).lt.0.)jf=1                                                     !/*wt4b        3*/
      if(tds(ip).lt.0.)call acetot(1,mk)                                        !/*wt         25*/
      lc=0                                                                      !/*wt         26*/
      do 130 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                    !/*wt         27*/
      iex=lme(llme+1,m)                                                         !/*wt         28*/
      if(rtc(krtc+6,iex).eq.erg.or.nty(lnty+iex).eq.3.or.mcal.ne.0)             !/*wt         29*/
     & go to 100                                                                !/*wt         30*/
      n=nxs(lnxs+3,iex)                                                         !/*wt         31*/
      rtc(krtc+11,iex)=-1.                                                      !/*wt4c        1*/
      if(nxs(lnxs+16,iex).eq.lc)go to 70                                        !/*wt         32*/
      ic=jxs(ljxs+1,iex)                                                        !/*wt         33*/
      ib=ic-1+n                                                                 !/*wt         34*/
      if(erg.lt.rtc(krtc+6,iex))ib=ic+ktc(kktc+1,iex)                           !/*wt         35*/
      if(erg.gt.rtc(krtc+6,iex))ic=ic+ktc(kktc+1,iex)-1                         !/*wt         36*/
   30 if(ib-ic.eq.1)go to 50                                                    !/*wt         37*/
      ih=(ic+ib)/2                                                              !/*wt         38*/
      if(erg.lt.xss(ih))go to 40                                                !/*wt         39*/
      ic=ih                                                                     !/*wt         40*/
      go to 30                                                                  !/*wt         41*/
   40 ib=ih                                                                     !/*wt         42*/
!        get the bin index and interpolation fraction.                          !/*wt4c        2*/
      go to 30                                                                  !/*wt         43*/
   50 ktc(kktc+1,iex)=ic-jxs(ljxs+1,iex)+1                                      !/*wt         44*/
      if(nty(lnty+iex).eq.2)go to 60                                            !/*wt         45*/
      rtc(krtc+1,iex)=max(zero,min(one,(erg-xss(ic))/(xss(ib)-xss(ic))))        !/*wt         46*/
!        sample cross sections from unresolved range probability tables.        !/*wt4c        3*/
      if(iunr.eq.0)go to 52                                                     !/*wt4c        4*/
      l=jxs(ljxs+23,iex)                                                        !/*wt4c        5*/
      if(l.eq.0)go to 52                                                        !/*wt4c        6*/
      if(erg.le.xss(l+6).or.erg.ge.xss(l+5+nint(xss(l))))go to 52               !/*wt4c        7*/
      call ures(ic)                                                             !/*wt4c        8*/
      rtc(krtc+8,iex)=rtc(krtc+12,iex)                                          !/*wt4c        9*/
      if(lfcl(llfc+icl).eq.0.or.jxs(ljxs+21,iex).eq.0)                          !/*wt4c       10*/
     & rtc(krtc+8,iex)=0.                                                       !/*wt4c       11*/
      if(rtc(krtc+8,iex).ne.0.)rtc(krtc+10,iex)=acenu(jxs(ljxs+2,iex))          !/*wt4c       12*/
      go to 90                                                                  !/*wt4c       13*/
!        get absorption and total cross sections.                               !/*wt4c       14*/
   52 rtc(krtc+3,iex)=xss(ic+2*n)+rtc(krtc+1,iex)*                              !/*wt4c       15*/
     & (xss(ic+2*n+1)-xss(ic+2*n))                                              !/*wt4c       16*/
      rtc(krtc+4,iex)=xss(ic+n)+rtc(krtc+1,iex)*(xss(ib+n)-xss(ic+n))           !/*wt4c       17*/
      go to 90                                                                  !/*wt         50*/
   60 lc=nxs(lnxs+16,iex)                                                       !/*wt         51*/
      id=ktc(kktc+1,iex)                                                        !/*wt         52*/
      go to 80                                                                  !/*wt         53*/
   70 ktc(kktc+1,iex)=id                                                        !/*wt         54*/
      ic=id+jxs(ljxs+1,iex)-1                                                   !/*wt         55*/
   80 rtc(krtc+3,iex)=xss(ic+2*n)                                               !/*wt         56*/
      rtc(krtc+4,iex)=xss(ic+n)                                                 !/*wt         57*/
   90 rtc(krtc+6,iex)=erg                                                       !/*wt         58*/
      rtc(krtc+7,iex)=-1                                                        !/*wt         59*/
  100 f=0.                                                                      !/*wt         60*/
      g=1.                                                                      !/*wt         61*/
      do 120 n=1,mod(int(abs(tds(ip))),10000000)                                !/*wt         62*/
      mt=tds(ip+n)                                                              !/*wt         63*/
      if(mt.ne.1000003)go to 110                                                !/*wt         64*/
      f=f+g                                                                     !/*wt         65*/
      g=1.                                                                      !/*wt         66*/
      go to 120                                                                 !/*wt         67*/
  110 g=g*getxs(mt)                                                             !/*wt         68*/
  120 continue                                                                  !/*wt         69*/
      h2=fme(lfme+m)*(f+g)                                                      !/*wt4c       18*/
      if(npert.ne.0)call pertc(2,mk,m,ip,-kk,h2)                                !/*wt4c       19*/
  130 h=h+h2                                                                    !/*wt4c       20*/
      go to 200                                                                 !/*wt         71*/
!                                                                               !/*wt         72*/
!        photon multipliers                                                     !/*wt         73*/
  140 do 190 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                    !/*wt         74*/
      iex=lme(llme+2,m)                                                         !/*wt         75*/
      if(erg.ne.rtc(krtc+6,iex).and.mcal.eq.0)call photot(mk)                   !/*wt         76*/
      f=0.                                                                      !/*wt         77*/
      g=1.                                                                      !/*wt         78*/
      do 180 n=1,mod(int(tds(ip)),10000000)                                     !/*wt         79*/
      if(tds(ip+n).ne.1000003)go to 150                                         !/*wt         80*/
      f=f+g                                                                     !/*wt         81*/
      g=1.                                                                      !/*wt         82*/
      go to 180                                                                 !/*wt         83*/
  150 j=-tds(ip+n)                                                              !/*wt         84*/
      if(mcal.ne.0)go to 160                                                    !/*wt         85*/
      if (j.gt.0) then                                                          !/*wtxk        1*/
        iex=lme(llme+2,m)                                                       !/*wtxk        2*/
      else                                                                      !/*wtxk        3*/
        xs=zero                                                                 !/*wtxk        4*/
        iex=lmn(llmn+m)                                                         !/*wtxk        5*/
        if(iex.eq.0.or.totpn.eq.zero)go to 170                                  !/*wtxk        6*/
        go to 160                                                               !/*wtxk        7*/
      endif                                                                     !/*wtxk        8*/
      xs=rtc(krtc+j,iex)                                                        !/*wt         86*/
      if(j.gt.1)xs=xs-rtc(krtc+j-1,iex)                                         !/*wt         87*/
      if(j.gt.4)xs=rtc(krtc+j-1,iex)                                            !/*wt         88*/
      go to 170                                                                 !/*wt         89*/
  160 xs=getxs(-j)                                                              !/*wt         90*/
  170 g=g*xs                                                                    !/*wt         91*/
  180 continue                                                                  !/*wt         92*/
      h2=fme(lfme+m)*(f+g)                                                      !/*wt4c       21*/
      if(npert.ne.0)call pertc(2,mk,m,ip,-kk,h2)                                !/*wt4c       22*/
  190 h=h+h2                                                                    !/*wt4c       23*/
      go to 200                                                                 !/*wtxl        2*/
!                                                                               !/*wtxl        3*/
!        proton multipliers                                                     !/*wtxl        4*/
  192 do 198 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                    !/*wtxl        5*/
      iex=lme(llme+9,m)                                                         !/*wtxl        6*/
      f=zero                                                                    !/*wtxl        7*/
      g=one                                                                     !/*wtxl        8*/
      do n=1,mod(int(tds(ip)),10000000)                                         !/*wtxl        9*/
         if(tds(ip+n).eq.1000003)then                                           !/*wtxl       10*/
            f=f+g                                                               !/*wtxl       11*/
            g=one                                                               !/*wtxl       12*/
         else                                                                   !/*wtxl       13*/
            j=tds(ip+n)                                                         !/*wtxl       14*/
            g=g*getxs(j)                                                        !/*wtxl       15*/
         endif                                                                  !/*wtxl       16*/
      enddo                                                                     !/*wtxl       17*/
  198 h=h+fme(lfme+m)*(f+g)                                                     !/*wtxl       18*/
!                                                                               !/*wt         94*/
!        find the attenuator associated with the multiplier bin.                !/*wt         95*/
  200 t=t*h                                                                     !/*wt         96*/
      if(abs(tds(ip)).lt.10000000)go to 270                                     !/*wt         97*/
      i=-1                                                                      !/*wt         98*/
      if(abs(tds(ip)).lt.20000000)i=1                                           !/*wt         99*/
      j=kk                                                                      !/*wt        100*/
  210 j=j+i                                                                     !/*wt        101*/
      ip=-tds(iptal(lipt+5,1,ital)+j)                                           !/*wt        102*/
      if(ip.le.1)go to 210                                                      !/*wt        103*/
      t=t*tds(iptal(lipt+5,2,ital)+j)                                           !/*wt        104*/
!                                                                               !/*wt        105*/
!        multiply tally weight for line-of-sight attenuator.                    !/*wt        106*/
  220 h=0.                                                                      !/*wt        107*/
      ttn=253e-10                                                               !/*wt        108*/
      do 260 n=1,int(tds(ip)),2                                                 !/*wt        109*/
      mk=tds(ip+n)                                                              !/*wt        110*/
      if(mk.eq.0)go to 260                                                      !/*wt        111*/
      if(mcal.ne.0)go to 230                                                    !/*wt        112*/
      if(ipt.eq.1)jf=1                                                          !/*wt4b        4*/
      if(ipt.eq.1)call acetot(1,mk)                                             !/*wt        113*/
      if(ipt.eq.2)call photot(mk)                                               !/*wt        114*/
      go to 250                                                                 !/*wt        115*/
  230 totm=0.                                                                   !/*wt        116*/
      do 240 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                    !/*wt        117*/
      iex=lme(llme+ipt,m)                                                       !/*wt        118*/
  240 totm=totm+getxs(1)*fme(lfme+m)                                            !/*wt        119*/
  250 h=h+tds(ip+n+1)*totm                                                      !/*wt        120*/
  260 continue                                                                  !/*wt        121*/
      if(h.gt.80.)t=0.                                                          !/*wt        122*/
      if(h.le.80.)t=t*exp(-h)                                                   !/*wt        123*/
!                                                                               !/*wt        124*/
!        restore the total cross section and thermal parameters.                !/*wt4b        5*/
  270 totm=s1                                                                   !/*wt        126*/
      ttn=t1                                                                    !/*wt        127*/
      if(jptal(ljpt+2,ital).ne.5.and.jptal(ljpt+2,ital).gt.3.and.               !/*wt4b        6*/
     & jf.ne.0)call acetot(0,mat(lmat+icl))                                     !/*wt4c       24*/
      return                                                                    !/*wt        128*/
!                                                                               !/*wt        129*/
!        set t=1. to tally tracks in f1 or collisions in f5.                    !/*wt        130*/
  280 if(ip.eq.-1)t=1.                                                          !/*wt4b        8*/
      if(ip.eq.-2)t=t/vel                                                       !/*wt4b        9*/
      return                                                                    !/*wt        132*/
      end                                                                       !/*wt        133*/
