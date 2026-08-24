!#ident mgxspt.F from deck mx                                                   !/*mx          1*/
      subroutine mgxspt(hn)                                                     !/*mx          2*/
!        process multigroup cross-section table iex.                            !/*mx          3*/
!        return 'total nu' or 'prompt nu' in hn if appropriate.                 !/*mx          4*/
      use allocate_memory                                                       !/*mxx         1*/
      use global_data                                                           !/*mxx         2*/
!                                                                               !/*mxx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*mxx         4*/
      implicit integer(kindi) (i-n)                                             !/*mxx         5*/
!                                                                               !/*mxx         6*/
      character hn*9,ht*10                                                      !/*mx          7*/
!                                                                               !/*mx          9*/
!        set up the particle type and the number of energy groups.              !/*mx         10*/
      call zaid(2,ht,ixl(lixl+1,iex))                                           !/*mx         11*/
      jp=max(1,nxs(lnxs+12,iex))                                                !/*mx         12*/
      jgm(jp)=nxs(lnxs+5,iex)                                                   !/*mx         13*/
      mgegbt(jp)=iex                                                            !/*mx         14*/
!                                                                               !/*mx         15*/
!        check use of tally cross sections by fm-card tallies.                  !/*mx         16*/
      do 70 it=1,ntal                                                           !/*mx         17*/
      iv=iptal(lipt+5,1,it)                                                     !/*mx         18*/
      if(iv.eq.0.or.ktp(lktp+jp,it).eq.0)go to 70                               !/*mx         19*/
      do 60 i=1,int(tds(iv))                                                    !/*mx         20*/
      ip=abs(tds(iv+i))                                                         !/*mx         21*/
      if(ip.le.1)go to 60                                                       !/*mx         22*/
      ii=tds(ip-1)                                                              !/*mx         23*/
      n2=1                                                                      !/*mx         24*/
      if(ii.lt.0)n2=tds(ip)                                                     !/*mx         25*/
      n1=1                                                                      !/*mx         26*/
      if(n2.eq.1)n1=mod(int(abs(tds(ip))),10000000)                             !/*mx         27*/
      i1=1                                                                      !/*mx         28*/
      do 50 j2=1,n2,2                                                           !/*mx         29*/
      if(n2.gt.1)ii=tds(ip+j2)                                                  !/*mx         30*/
      do 40 m=jmd(ljmd+ii),jmd(ljmd+ii+1)-1                                     !/*mx         31*/
      if(lme(llme+jp,m).ne.iex)go to 40                                         !/*mx         32*/
      do 30 j=1,n1                                                              !/*mx         33*/
      if(n2.eq.1)i1=tds(ip+j)                                                   !/*mx         34*/
      if(i1.eq.1000003)go to 30                                                 !/*mx         35*/
      i2=max(min(-i1,7),1)                                                      !/*mx4b        1*/
      if(i1.eq.-i2.and.jxs(ljxs+1+i2,iex).ne.0)call erprnt(1,2,1,i1,0,          !/*mx4b        2*/
     & 0,0,0,'13hreaction mt =,i3,28h has new meaning in ver. 4b.')             !/*mx4b        3*/
      if(i1.eq.-i2.and.jxs(ljxs+1+i2,iex).ne.0)go to 30                         !/*mx4b        4*/
      do 10 k=1,nxs(lnxs+4,iex)                                                 !/*mx         37*/
   10 if(abs(nint(xss(jxs(ljxs+9,iex)-1+k))).eq.i1)go to 20                     !/*mxx         7*/
      call erprnt(1,2,2,i1,jptal(ljpt+1,it),0,0,0,                              !/*mx4b        5*/
     & '9hreaction ,i3,33h for'//ht//' does not exist. fm,i3')                  !/*mx4b        6*/
      go to 30                                                                  !/*mx         41*/
   20 xss(jxs(ljxs+9,iex)-1+k)=-abs(nint(xss(jxs(ljxs+9,iex)-1+k)))             !/*mxx         8*/
   30 continue                                                                  !/*mx         43*/
   40 continue                                                                  !/*mx         44*/
   50 continue                                                                  !/*mx         45*/
   60 continue                                                                  !/*mx         46*/
   70 continue                                                                  !/*mx         47*/
!                                                                               !/*mx         48*/
!        check use of tally cross sections by heating tallies.                  !/*mx         49*/
      do 140 it=1,ntal                                                          !/*mx         50*/
      n1=jptal(ljpt+2,it)-5                                                     !/*mx         51*/
      if(n1.lt.1.or.n1.ge.3.or.ktp(lktp+jp,it).eq.0)go to 140                   !/*mx4b        7*/
      do 130 l=1,iptal(lipt+1,3,it)                                             !/*mx         53*/
      ip=itds(iptal(lipt+1,1,it)+l)                                             !/*mx         54*/
      do 120 j2=1,itds(ip)                                                      !/*mx         55*/
      j=mat(lmat+itds(ip+j2))                                                   !/*mx         56*/
      do 110 m=jmd(ljmd+j),jmd(ljmd+j+1)-1                                      !/*mx         57*/
      if(lme(llme+jp,m).ne.iex)go to 110                                        !/*mx         58*/
      do 100 j=1,n1                                                             !/*mx         59*/
      i1=401                                                                    !/*mx         60*/
      if(n1.eq.2.and.j.eq.1)i1=18                                               !/*mx         61*/
      if(n1.eq.2.and.j.eq.2)i1=318                                              !/*mx         62*/
      do 80 k=1,nxs(lnxs+4,iex)                                                 !/*mx         63*/
   80 if(abs(nint(xss(jxs(ljxs+9,iex)-1+k))).eq.i1)go to 90                     !/*mxx         9*/
      call erprnt(1,2,2,i1,jptal(ljpt+1,it),0,0,0,                              !/*mx4b        8*/
     & '9hreaction ,i3,33h for'//ht//' does not exist.  f,i3')                  !/*mx4b        9*/
      go to 100                                                                 !/*mx         67*/
   90 xss(jxs(ljxs+9,iex)-1+k)=-abs(nint(xss(jxs(ljxs+9,iex)-1+k)))             !/*mxx        10*/
  100 continue                                                                  !/*mx         69*/
  110 continue                                                                  !/*mx         70*/
  120 continue                                                                  !/*mx         71*/
  130 continue                                                                  !/*mx         72*/
  140 continue                                                                  !/*mx         73*/
!                                                                               !/*mx         74*/
!        expunge unwanted tally cross sections.                                 !/*mx         75*/
      n=0                                                                       !/*mx         76*/
      do 160 k=1,nxs(lnxs+4,iex)                                                !/*mx         77*/
      if(nint(xss(jxs(ljxs+9,iex)-1+k)).gt.0)go to 160                          !/*mxx        11*/
      n=n+1                                                                     !/*mx         79*/
      xss(jxs(ljxs+9,iex)-1+n)=-nint(xss(jxs(ljxs+9,iex)-1+k))                  !/*mxx        12*/
      do 150 i=1,nxs(lnxs+5,iex)                                                !/*mx         81*/
  150 xss(jxs(ljxs+10,iex)-1+(n-1)*nxs(lnxs+5,iex)+i)=                          !/*mx         82*/
     & xss(jxs(ljxs+10,iex)-1+(k-1)*nxs(lnxs+5,iex)+i)                          !/*mx         83*/
  160 continue                                                                  !/*mx         84*/
      call exmg(jxs(ljxs+9,iex)+n,nxs(lnxs+4,iex)-n)                            !/*mx         85*/
      call exmg(jxs(ljxs+10,iex)+n*nxs(lnxs+5,iex),                             !/*mx         86*/
     & nxs(lnxs+5,iex)*(nxs(lnxs+4,iex)-n))                                     !/*mx         87*/
      nxs(lnxs+4,iex)=n                                                         !/*mx         88*/
!                                                                               !/*mx         89*/
!        check and expunge secondary production cross sections.                 !/*mx         90*/
      ms=1                                                                      !/*mx         91*/
      do 180 ip=1,mipt                                                          !/*mx         92*/
      if(kpt(ip).eq.0.or.ip.eq.jp.or.ip.eq.1)go to 180                          !/*mx         93*/
      do 170 i=1,nxs(lnxs+8,iex)                                                !/*mx         94*/
  170 if(nint(xss(jxs(ljxs+11,iex)+i-1)).eq.ip)go to 180                        !/*mxx        13*/
      call erprnt(1,2,0,0,0,0,0,0,                                              !/*mx         96*/
     & '54hno '//hnp(ip)//' production cross sections in '//ht)                 !/*mxx        14*/
      ms=0                                                                      !/*mx         98*/
  180 continue                                                                  !/*mx         99*/
      do 210 k=1,nxs(lnxs+8,iex)                                                !/*mx        100*/
      if(kpt(nint(xss(jxs(ljxs+11,iex)+k-1))).ne.0)go to 210                    !/*mxx        15*/
      l=jxs(ljxs+1,iex)-1+nint(xss(jxs(ljxs+16,iex)+k))                         !/*mx4c        1*/
      n=nint(xss(jxs(ljxs+1,iex)-1+nint(xss(jxs(ljxs+12,iex)+k-1))))            !/*mxx        16*/
      if(nint(xss(jxs(ljxs+16,iex)+k)).eq.0)go to 200                           !/*mxx        17*/
      m=0                                                                       !/*mx        105*/
      do 190 i=1,n*nxs(lnxs+5,iex)                                              !/*mx        106*/
  190 m=max(m,nint(xss(l+i-1)))                                                 !/*mxx        18*/
      i=m+nint(xss(jxs(ljxs+15,iex)+k-1))-1                                     !/*mx4c        3*/
      if(m.gt.0)                                                                !/*mx        109*/
     & call exmg(jxs(ljxs+1,iex)-1+nint(xss(jxs(ljxs+17,iex)+k)),i)             !/*mx4c        4*/
      call exmg(l,n*nxs(lnxs+5,iex))                                            !/*mx        111*/
  200 call exmg(jxs(ljxs+1,iex)-1+nint(xss(jxs(ljxs+13,iex)+k)),                !/*mx4c        5*/
     & n*nxs(lnxs+5,iex))                                                       !/*mx        113*/
      call exmg(jxs(ljxs+1,iex)-1+nint(xss(jxs(ljxs+12,iex)+k-1)),2*n+1)        !/*mx4c        6*/
  210 continue                                                                  !/*mx        115*/
      n=0                                                                       !/*mx        116*/
      do 220 k=1,nxs(lnxs+8,iex)                                                !/*mx        117*/
      if(kpt(nint(xss(jxs(ljxs+11,iex)+k-1))).eq.0)go to 220                    !/*mxx        19*/
      n=n+1                                                                     !/*mx        119*/
      xss(jxs(ljxs+11,iex)+n-1)=nint(xss(jxs(ljxs+11,iex)+k-1))                 !/*mxx        20*/
      xss(jxs(ljxs+12,iex)+n-1)=nint(xss(jxs(ljxs+12,iex)+k-1))                 !/*mxx        21*/
      xss(jxs(ljxs+13,iex)+n)=nint(xss(jxs(ljxs+13,iex)+k))                     !/*mxx        22*/
      xss(jxs(ljxs+14,iex)+n-1)=nint(xss(jxs(ljxs+14,iex)+k-1))                 !/*mxx        23*/
      xss(jxs(ljxs+15,iex)+n-1)=nint(xss(jxs(ljxs+15,iex)+k-1))                 !/*mxx        24*/
      xss(jxs(ljxs+16,iex)+n)=nint(xss(jxs(ljxs+16,iex)+k))                     !/*mxx        25*/
      xss(jxs(ljxs+17,iex)+n)=nint(xss(jxs(ljxs+17,iex)+k))                     !/*mxx        26*/
  220 continue                                                                  !/*mx        127*/
      nxs(lnxs+8,iex)=n                                                         !/*mx        128*/
!                                                                               !/*mx        129*/
!        expunge prompt or total nubar table.                                   !/*mx        130*/
      if(nxs(lnxs+10,iex).eq.0)go to 230                                        !/*mx        131*/
      if(nxs(lnxs+10,iex).eq.1)call erprnt(1,2,0,0,0,0,0,0,                     !/*mx        132*/
     & '50hnubar of '//ht//' may be either prompt or total.')                   !/*mx        133*/
      if(nxs(lnxs+10,iex).eq.1)go to 230                                        !/*mx        134*/
      if(itotnu.ne.1)call exmg(jxs(ljxs+4,iex)+nxs(lnxs+5,iex),                 !/*mx        135*/
     & nxs(lnxs+5,iex))                                                         !/*mx        136*/
      if(itotnu.ne.1)hn='prompt nu'                                             !/*mx        137*/
      if(itotnu.eq.1)call exmg(jxs(ljxs+4,iex),nxs(lnxs+5,iex))                 !/*mx        138*/
      if(itotnu.eq.1)hn='total nu'                                              !/*mx        139*/
      nxs(lnxs+10,iex)=1                                                        !/*mx        140*/
!                                                                               !/*mx        141*/
!        create table of indexes into p0 and xpn tables.                        !/*mx        142*/
  230 m=1                                                                       !/*mx        143*/
      if(mcal.eq.1)m=2                                                          !/*mx        144*/
      if(mcal.eq.1.and.kpt(1).ne.0.and.kpt(2).ne.0.and.jp.eq.1.and.             !/*mx        145*/
     & ms.ne.0)m=3                                                              !/*mx        146*/
      mc=nxs(lnxs+1,iex)+m*nxs(lnxs+5,iex)+4                                    !/*mxx        27*/
      mxs=size(xss)                                                             !/*mxx        28*/
      if(mc.gt.mxs) then                                                        !/*mxx        29*/
        xss => chg_mem (xss,mc,is)                                              !/*mxx        30*/
        if(is.ne.0)call expire(0,'mgxspt','chg_mem error')                      !/*mxx        31*/
      endif                                                                     !/*mxx        32*/
      jxs(ljxs+18,iex)=jxs(ljxs+1,iex)+nxs(lnxs+1,iex)                          !/*mx        149*/
      xss(jxs(ljxs+18,iex))=1                                                   !/*mx        150*/
      do 240 j=1,nxs(lnxs+5,iex)-1                                              !/*mx        151*/
  240 xss(jxs(ljxs+18,iex)+j)=nint(xss(jxs(ljxs+18,iex)+j-1))+                  !/*mxx        33*/
     & min(j,nxs(lnxs+6,iex)+1)+min(nxs(lnxs+5,iex)-j,nxs(lnxs+7,iex))          !/*mx        153*/
!                                                                               !/*mx        154*/
!        add up the scattering and photon production cross sections.            !/*mx        155*/
      if(m.eq.1)go to 290                                                       !/*mx        156*/
      jxs(ljxs+19,iex)=jxs(ljxs+18,iex)+nxs(lnxs+5,iex)                         !/*mx        157*/
      n=0                                                                       !/*mx        158*/
      l=jxs(ljxs+1,iex)-1+nint(xss(jxs(ljxs+13,iex)))                           !/*mx4c        7*/
      if(m.ne.3)go to 250                                                       !/*mx        160*/
      jxs(ljxs+20,iex)=jxs(ljxs+19,iex)+nxs(lnxs+5,iex)                         !/*mx        161*/
      n=nint(xss(jxs(ljxs+1,iex)-1+nint(xss(jxs(ljxs+12,iex)))))                !/*mxx        34*/
      k=jxs(ljxs+1,iex)+nint(xss(jxs(ljxs+13,iex)+1))-2                         !/*mx4c        9*/
  250 do 260 i=1,nxs(lnxs+5,iex)*(m-1)                                          !/*mx        164*/
  260 xss(jxs(ljxs+19,iex)-1+i)=0.                                              !/*mx        165*/
      do 280 j=1,nxs(lnxs+5,iex)                                                !/*mx        166*/
      do 270 i=1,min(j,nxs(lnxs+6,iex)+1)+min(nxs(lnxs+5,iex)-j,                !/*mx        167*/
     & nxs(lnxs+7,iex))                                                         !/*mx        168*/
      xss(jxs(ljxs+19,iex)-1+j)=xss(jxs(ljxs+19,iex)-1+j)+xss(l)                !/*mx        169*/
  270 l=l+1                                                                     !/*mx        170*/
      do 280 i=1,n                                                              !/*mx        171*/
  280 xss(jxs(ljxs+20,iex)-1+j)=xss(jxs(ljxs+20,iex)-1+j)+                      !/*mx        172*/
     & xss(k+(j-1)*n+i)                                                         !/*mx        173*/
  290 nxs(lnxs+1,iex)=nxs(lnxs+1,iex)+m*nxs(lnxs+5,iex)                         !/*mx        174*/
!                                                                               !/*mx        175*/
!        for an adjoint problem, optionally adjust data by group widths.        !/*mx        176*/
      if(mcal.ne.2.or.img.eq.0)return                                           !/*mx        177*/
      k=jxs(ljxs+1,iex)+nint(xss(jxs(ljxs+13,iex)))-2                           !/*mx4c       10*/
      l=1                                                                       !/*mx        179*/
      do 320 i=1,nxs(lnxs+5,iex)                                                !/*mx        180*/
      d=xss(jxs(ljxs+1,iex)+nxs(lnxs+5,iex)+i-1)                                !/*mx        181*/
      if(jxs(ljxs+5,iex).eq.0)go to 300                                         !/*mx        182*/
      xss(jxs(ljxs+5,iex)+i-1)=xss(jxs(ljxs+5,iex)+i-1)/d                       !/*mx        183*/
      xss(jxs(ljxs+4,iex)+i-1)=xss(jxs(ljxs+4,iex)+i-1)*d                       !/*mx        184*/
  300 do 310 j=max(1,i-nxs(lnxs+6,iex)),min(nxs(lnxs+5,iex),                    !/*mx        185*/
     & i+nxs(lnxs+7,iex))                                                       !/*mx        186*/
      xss(k+l)=xss(k+l)*d/xss(jxs(ljxs+1,iex)+nxs(lnxs+5,iex)+j-1)              !/*mx        187*/
  310 l=l+1                                                                     !/*mx        188*/
      do 320 m=1,nxs(lnxs+8,iex)                                                !/*mx        189*/
      l2=jxs(ljxs+1,iex)+nint(xss(jxs(ljxs+12,iex)+m-1))-1                      !/*mx4c       11*/
      k2=jxs(ljxs+1,iex)+nint(xss(jxs(ljxs+13,iex)+m))-2                        !/*mx4c       12*/
      do 320 j=1,int(xss(l2))                                                   !/*mx        192*/
  320 xss(k2+j)=xss(k2+j)*d/xss(l2+nint(xss(l2))+j)                             !/*mx4c       13*/
      return                                                                    !/*mx        194*/
      end                                                                       !/*mx        195*/
