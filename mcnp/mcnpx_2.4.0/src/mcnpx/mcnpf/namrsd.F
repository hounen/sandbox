!#ident namrsd.F from deck nr4b                                                 !/*nr4b        1*/
      function namrsd(ir,jj,jc,hh,mk,jf)                                        !/*nrxa        1*/
!        get the name of the repeated structures source distribution            !/*nr4b        3*/
!        cell number or other distribution value.                               !/*nr4b        4*/
!          ir = source distribution number                                      !/*nr4b        5*/
!          jj = bin number in source distribution                               !/*nr4b        6*/
!          jc = 1 call from prsdst; = 2 call from volume.                       !/*nr4b        7*/
      use global_data                                                           !/*nrx         1*/
!                                                                               !/*nrx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*nrx         3*/
      implicit integer(kindi) (i-n)                                             !/*nrx         4*/
!                                                                               !/*nrx         5*/
      character hh*(*),hs*120,ha*8                                              !/*nrxa        2*/
      k(i)=ksd(lksd+i,ir)                                                       !/*nr4b        9*/
      s(i,j)=spf(k(13)+i,j)                                                     !/*nr4b       10*/
!                                                                               !/*nr4b       11*/
      m=s(1,jj)                                                                 !/*nr4b       12*/
      namrsd=m                                                                  !/*nr4b       13*/
      if(m.lt.0)go to 20                                                        !/*nr4b       14*/
      if(k(6).ne.0)go to 10                                                     !/*nr4b       15*/
!                                                                               !/*nr4b       16*/
!        value of distribution is usually namrsd=m unless                       !/*nr4b       17*/
!        called from prsdst (jc=1) from volume (sp v option).                   !/*nr4b       18*/
      if(k(10).ne.0.and.jc.eq.1)namrsd=ncl(lncl+m)                              !/*nr4b       19*/
      return                                                                    !/*nr4b       20*/
!                                                                               !/*nr4b       21*/
!        value of distribution is a distribution number.                        !/*nr4b       22*/
   10 namrsd=ksd(lksd+1,m)                                                      !/*nr4b       23*/
      return                                                                    !/*nr4b       24*/
!                                                                               !/*nr4b       25*/
!        m<0: value of distribution is cell in repeated structure.              !/*nr4b       26*/
   20 l=-m-1                                                                    !/*nr4b       27*/
      n=kcp(l)                                                                  !/*nrxc        1*/
!                                                                               !/*nrxc        2*/
!        if kcp(l-1)>0:  kcp array after jsourc conversion.                     !/*nrxc        3*/
!          then k(10)>0 (sp v option);  called from volume (jc=2) or            !/*nrxc        4*/
!          from volume/prsdst jc=1).                                            !/*nrxc        5*/
      if(kcp(l-1).ge.0)then                                                     !/*nrxc        6*/
      n=1                                                                       !/*nrxc        7*/
      do 30 i=1,kcp(l)                                                          !/*nrxc        8*/
      if(kcp(l+n+1).eq.1000001)n=n+5                                            !/*nrxc        9*/
   30 n=n+1                                                                     !/*nrxc       10*/
      namrsd=kcp(l+n)                                                           !/*nrxc       11*/
      if(jc.eq.1)namrsd=ncl(lncl+kcp(l+n))                                      !/*nrxc       12*/
      if(kcp(l+n).gt.100000)call erprnt(1,1,1,ksd(lksd+1,ir),0,0,0,0,           !/*nrxc       13*/
     & '24hvolumes for source dist.,i4,23h can not be determined.')             !/*nrxc       14*/
      else                                                                      !/*nrxc       15*/
!                                                                               !/*nrxc       16*/
!        if kcp(l-1)=-1:  kcp array before jsourc conversion.                   !/*nrxc       17*/
!          then k(10)=0 and namrsd called from prsdst from jsourc.              !/*nrxc       18*/
      n=kcp(l)                                                                  !/*nrxc       19*/
      namrsd=kcp(l+n)                                                           !/*nrxc       20*/
      if(namrsd.eq.1000002)namrsd=kcp(l+n-5)                                    !/*nrxc       21*/
      if(namrsd.gt.100000)namrsd=ksd(lksd+1,namrsd-100000)                      !/*nrxc       22*/
      endif                                                                     !/*nrxc       23*/
!                                                                               !/*nrxc       24*/
!        put cell path in xx:xx(xx xx xx):xx notation in hs.                    !/*nrxc       25*/
      if(namrsd.lt.100000.and.n.gt.1.and.l+n.lt.lkcp+mk.and.                    !/*nrxc       26*/
     & kcp(l+n+1).eq.1000008.and.jc.eq.1)jf=1                                   !/*nrxc       27*/
      if(jf.ne.1)return                                                         !/*nrxc       28*/
      ka=1                                                                      !/*nrxc       29*/
      ja=0                                                                      !/*nrxc       30*/
      do 60 ij=l+1,l+n                                                          !/*nrxc       31*/
      if(kcp(ij).gt.1000000)go to 50                                            !/*nrxc       32*/
      ha=' '                                                                    !/*nrxc       33*/
      write(ha,'(i8)')kcp(ij)                                                   !/*nrxc       34*/
      if(ja.eq.0.and.kcp(l-1).ge.0)write(ha,'(i8)')ncl(lncl+kcp(ij))            !/*nrxc       35*/
      do 40 kb=1,8                                                              !/*nrxc       36*/
      if(ha(kb:kb).eq.' ')go to 40                                              !/*nrxc       37*/
      hs(ka:ka)=ha(kb:kb)                                                       !/*nrxc       38*/
      ka=ka+1                                                                   !/*nrxc       39*/
   40 continue                                                                  !/*nrxc       40*/
      if(ja.eq.1.or.ja.eq.2)hs(ka:ka)=' '                                       !/*nrxc       41*/
      if(ja.eq.1.or.ja.eq.2)ka=ka+1                                             !/*nrxc       42*/
      if(ja.ne.0)ja=ja+1                                                        !/*nrxc       43*/
      if(ja.eq.0.and.ij.le.l+n-1.and.kcp(ij+1).lt.1000000)hs(ka:ka)=':'         !/*nrxc       44*/
      if(ja.eq.0.and.ij.le.l+n-1.and.kcp(ij+1).lt.1000000)ka=ka+1               !/*nrxc       45*/
      go to 60                                                                  !/*nrxc       46*/
   50 ja=1                                                                      !/*nrxc       47*/
      if(kcp(ij).eq.1000001)hs(ka:ka)='('                                       !/*nrxc       48*/
      if(kcp(ij).eq.1000002)hs(ka:ka)=')'                                       !/*nrxc       49*/
      ka=ka+1                                                                   !/*nrxc       50*/
      if(kcp(ij).ne.1000002)go to 60                                            !/*nrxc       51*/
      ja=0                                                                      !/*nrxc       52*/
      if(ij.le.l+n-1)hs(ka:ka)=':'                                              !/*nrxc       53*/
      if(ij.le.l+n-1)ka=ka+1                                                    !/*nrxc       54*/
   60 continue                                                                  !/*nrxc       55*/
      ka=ka-1                                                                   !/*nrxc       56*/
      kb=1                                                                      !/*nrxc       57*/
      if(ka.le.80)go to 80                                                      !/*nrxc       58*/
      do 70 kb=1,ka                                                             !/*nrxc       59*/
   70 if(hs(kb:kb).eq.':'.and.ka-kb+1.le.80)go to 80                            !/*nrxc       60*/
   80 kc=1                                                                      !/*nrxc       61*/
      if(ka-kb+1.le.16)kc=17-(ka-kb+1)                                          !/*nrxc       62*/
!                                                                               !/*nrxc       63*/
!        put path, hs, in hh in xx<xx[xx xx xx]<xx notation.                    !/*nrxc       64*/
      hh=' '                                                                    !/*nrxc       65*/
      jf=kc+ka-kb                                                               !/*nrxc       66*/
      kz=kb-1                                                                   !/*nrxc       67*/
      ky=jf+1                                                                   !/*nrxc       68*/
      do 90 kh=kb,ka                                                            !/*nrxc       69*/
      if(hs(kh:kh).eq.'(')hs(kh:kh)='['                                         !/*nrxc       70*/
      if(hs(kh:kh).eq.')')hs(kh:kh)=']'                                         !/*nrxc       71*/
      if(hs(kh:kh).ne.':')go to 90                                              !/*nrxc       72*/
      ky=ky+kz-kh                                                               !/*nrxc       73*/
      hh(ky+1:ky-kz+kh-1)=hs(kz+1:kh-1)                                         !/*nrxc       74*/
      hh(ky:ky)='<'                                                             !/*nrxc       75*/
      kz=kh                                                                     !/*nrxc       76*/
   90 continue                                                                  !/*nrxc       77*/
      hh(ky+kz-ka:ky-1)=hs(kz+1:ka)                                             !/*nrxc       78*/
      return                                                                    !/*nr4b       48*/
      end                                                                       !/*nr4b       49*/
