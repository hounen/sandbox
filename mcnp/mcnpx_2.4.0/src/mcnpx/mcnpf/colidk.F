!#ident colidk.F from deck cz                                                   !/*cz          1*/
      subroutine colidk                                                         !/*cz          2*/
!        process the fission part of a kcode collision.                         !/*cz          3*/
      use global_data                                                           !/*czx         1*/
!                                                                               !/*czx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*czx         3*/
      implicit integer(kindi) (i-n)                                             !/*czx         4*/
!                                                                               !/*czx         5*/
!                                                                               !/*cz          5*/
!        get collision estimates (summed over nuclides) of                      !/*cz4b        1*/
!        siga (ab), sigf (af), and nu*sigf (fm).                                !/*cz4b        2*/
!        note: tgp used as dynamically dimensioned scratch space.               !/*cz4b        3*/
      fm=0.                                                                     !/*cz4b        4*/
      ab=0.                                                                     !/*cz          7*/
      af=0.                                                                     !/*cz4a        1*/
      mk=mat(lmat+icl)                                                          !/*cz          8*/
      do 10 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                     !/*cz          9*/
      t1=fme(lfme+m)*rtc(krtc+8,lme(llme+1,m))                                  !/*cz4b        5*/
      fm=fm+t1*rtc(krtc+10,lme(llme+1,m))                                       !/*cz4b        6*/
      tgp(ktgp+m+1-jmd(ljmd+mk))=fm                                             !/*cz4b        7*/
      af=af+t1                                                                  !/*cz4b        8*/
   10 ab=ab+fme(lfme+m)*rtc(krtc+3,lme(llme+1,m))                               !/*cz4b        9*/
!                                                                               !/*cz4b       10*/
!        get collision estimate of prompt neutron removal lifetime.             !/*cz4b       11*/
      rh=rho(lrho+icl)                                                          !/*cz4c        1*/
      a=wgt/totm                                                                !/*czx         8*/
      rlttc(1,1)=rlttc(1,1)+(ab+af)*a*tme                                       !/*cz4b       13*/
      rlttc(1,2)=rlttc(1,2)+(ab+af)*a                                           !/*cz4b       14*/
!                                                                               !/*cz         25*/
!        increment k tallies and summary tables if fission is possible.         !/*cz         26*/
      if(fm.eq.0.)go to 170                                                     !/*cz4c        3*/
      xf=fm*a                                                                   !/*cz         28*/
      if(krflg.ne.2)sumktc(1)=sumktc(1)+xf                                      !/*cz         30*/
!                                                                               !/*cz         31*/
!        sample the fissioning nuclide.                                         !/*cz         32*/
      ih=iex                                                                    !/*cz         33*/
      t1=rang()*fm                                                              !/*cz         34*/
      k=0                                                                       !/*cz         35*/
   30 k=k+1                                                                     !/*cz         36*/
      if(t1.gt.tgp(ktgp+k))go to 30                                             !/*cz         37*/
      iex=lme(llme+1,jmd(ljmd+mk)+k-1)                                          !/*cz         38*/
      ktc(kktc+2,iex)=ktc(kktc+1,iex)                                           !/*cz         40*/
      rtc(krtc+2,iex)=rtc(krtc+1,iex)                                           !/*cz         41*/
!                                                                               !/*cz         44*/
!        store the new source points, if any.                                   !/*cz         45*/
      n1=xf/rkk+rang()                                                          !/*cz4b       15*/
#ifdef MULTT                                                                    /*!czxk        1*/
      call thread_lockon(jlock)                                                 !/*czxk        2*/
#endif                                                                          /*!czxk        3*/
      if(n1.eq.0)go to 160                                                      !/*cz         48*/
      nss=nss+n1                                                                !/*cz         49*/
      if(kcsf.eq.1)go to 160                                                    !/*cz         50*/
      do 130 n=1,n1                                                             !/*cz         51*/
      if(ist.eq.ixak.and.nss.ne.n1)go to 140                                    !/*cz         52*/
      l=lfso+6*(ist-1)                                                          !/*cz         53*/
      if(lev.eq.0)go to 70                                                      !/*cz         54*/
      fso(l+5)=udt(7,0)                                                         !/*cz         55*/
      if(lat(llat+1,int(udt(7,0))).eq.0)go to 60                                !/*cz         56*/
      m=lat(llat+2,int(udt(7,0)))                                               !/*cz         57*/
      do 50 i=1,3                                                               !/*cz         58*/
   50 fso(l+i)=udt(i,0)+udt(8,0)*vcl(lvcl+i,1,m)+                               !/*cz         59*/
     & udt(9,0)*vcl(lvcl+i,2,m)+udt(10,0)*vcl(lvcl+i,3,m)                       !/*cz         60*/
      go to 90                                                                  !/*cz         61*/
   60 fso(l+1)=udt(1,0)                                                         !/*cz         62*/
      fso(l+2)=udt(2,0)                                                         !/*cz         63*/
      fso(l+3)=udt(3,0)                                                         !/*cz         64*/
      go to 90                                                                  !/*cz         65*/
   70 fso(l+1)=xxx                                                              !/*cz         66*/
      fso(l+2)=yyy                                                              !/*cz         67*/
      fso(l+3)=zzz                                                              !/*cz         68*/
      fso(l+5)=icl                                                              !/*cz         69*/
      if(junf.eq.0)go to 90                                                     !/*cor4-2    204*/
      if(lat(llat+1,icl).eq.0)go to 90                                          !/*cz         70*/
      m=lat(llat+2,icl)                                                         !/*cz         71*/
      do 80 i=1,3                                                               !/*cz         72*/
   80 fso(l+i)=gpblcm(i)+iii*vcl(lvcl+i,1,m)+jjj*vcl(lvcl+i,2,m)+               !/*cz         73*/
     & kkk*vcl(lvcl+i,3,m)                                                      !/*cz         74*/
   90 if(mcal.ne.0)go to 100                                                    !/*cz         75*/
      call acecol(2)                                                            !/*cz         76*/
      if(kdb.ne.0)return                                                        !/*cz         77*/
      fso(l+4)=colout(1,1)                                                      !/*cz         78*/
      go to 120                                                                 !/*cz         79*/
  100 t=rang()                                                                  !/*cz         80*/
      i=0                                                                       !/*cz         81*/
  110 t=t-xss(jxs(ljxs+5,iex)+i)                                                !/*cz         82*/
      i=i+1                                                                     !/*cz         83*/
      if(t.gt.0.)go to 110                                                      !/*cz         84*/
      fso(l+4)=i                                                                !/*cz         85*/
  120 fso(l+6)=npstc                                                            !/*cz         86*/
      if(ist.eq.msrk)ist=0                                                      !/*cz         87*/
  130 ist=ist+1                                                                 !/*cz         88*/
      go to 160                                                                 !/*cz         89*/
!                                                                               !/*cz         90*/
!        handle the case of the new source overrunning the old source.          !/*cz         91*/
  140 if(kcy.le.1)go to 145                                                     !/*cz4c        5*/
#ifdef MULTT                                                                    /*!czxk        4*/
      call thread_lockoff(jlock)                                                !/*czxk        5*/
#endif                                                                          /*!czxk        6*/
      if(kcsf.eq.0)call expirx(1,'colidk',                                      !/*cz4c        7*/
     & 'the new source has overrun the old source.')                            !/*cz         93*/
      if(kcsf.ne.0)call expirx(1,'colidk',                                      !/*cz4c        8*/
     & ' the new source has overrun the old source again.')                     !/*cz         95*/
      return                                                                    !/*cz4c        9*/
  145 if(kcsf.eq.0)write(iuo,150)                                               !/*cz4c       10*/
      if(kcsf.eq.0)write(jtty,150)                                              !/*cz         98*/
  150 format(35h new source has overrun old source.,                            !/*cz         99*/
     & 32h  will start over with larger k.)                                     !/*cz        100*/
      kcsf=1                                                                    !/*cz        101*/
!                                                                               !/*cz        102*/
!         restore the index of the collision nucleus.                           !/*cz        103*/
  160 iex=ih                                                                    !/*cz        104*/
#ifdef MULTT                                                                    /*!czxk        7*/
      call thread_lockoff(jlock)                                                !/*czxk        8*/
#endif                                                                          /*!czxk        9*/
  170 continue                                                                  !/*czx        19*/
      return                                                                    !/*cz        106*/
      end                                                                       !/*cz        107*/
