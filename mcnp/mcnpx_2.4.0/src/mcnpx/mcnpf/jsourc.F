!#ident jsourc.F from deck js                                                   !/*js          1*/
      subroutine jsourc                                                         !/*js          2*/
!        process the specifications of the general source.                      !/*js          3*/
      use global_data                                                           !/*jsx         1*/
      use imcn_data                                                             !/*jsx         2*/
!                                                                               !/*jsx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*jsx         4*/
      implicit integer(kindi) (i-n)                                             !/*jsx         5*/
!                                                                               !/*jsx         6*/
      character ht*12                                                           !/*js          6*/
      parameter (maxf=16)                                                       !/*jsx         7*/
      dimension ij(maxf),js(20),ko(maxf,maxf),mr(maxf),ms(20)                   !/*js          7*/
      data ij/1,2,14,12,10,11,7,8,9,6,13,15,5,16,3,4/                           !/*js          8*/
      k(i,j)=ksd(lksd+i,j)                                                      !/*js          9*/
!                                                                               !/*js         10*/
!        set non-zero default values.                                           !/*js         11*/
      if(ksdef.eq.0.or.idefv(3).ne.0)go to 10                                   !/*js         12*/
      msd=msd+1                                                                 !/*js         13*/
      ksd(lksd+1,msd)=9999                                                      !/*js4a        1*/
      ksd(lksd+2,msd)=-3                                                        !/*js         15*/
      ivdis(3)=9999                                                             !/*js4a        2*/
      idefv(3)=1                                                                !/*js         17*/
   10 kr=1                                                                      !/*js         18*/
      if(nsr.eq.6)kr=2                                                          !/*js         19*/
      if(nsr.eq.6)go to 20                                                      !/*js         20*/
      if(idefv(3).eq.0)srv(1,3)=14.                                             !/*js         21*/
      if(idefv(15).eq.0)srv(1,15)=1.                                            !/*js         22*/
      if(isfiss.ne.0)then                                                       !/*jsxk        1*/
         idefv(3)=0                                                             !/*jsxk        2*/
         srv(1,3)=two                                                           !/*jsxk        3*/
      endif                                                                     !/*jsxk        4*/
   20 if(idefv(17).eq.0)srv(1,17)=1.                                            !/*js         23*/
      if(idefv(18).eq.0.)srv(1,18)=.01                                          !/*js         24*/
!                                                                               !/*js         25*/
!        print values of defaulted or explicitly defined variables.             !/*js         26*/
      if(ink(10).eq.0)go to 70                                                  !/*js         27*/
      write(iuo,30)                                                             !/*js         28*/
   30 format(7h1source,97x,14hprint table 10//20h values of defaulted,          !/*js4a        3*/
     & 39h or explicitly defined source variables/)                             !/*js         30*/
      write(ht,'(1pe12.4)')srv(1,5)                                             !/*js         31*/
      if(idefv(5).eq.0.and.ivdis(2).eq.0)ht='  cosine'                          !/*js         32*/
      if(idefv(5).eq.0.and.idefv(2).eq.0)ht='  isotropic'                       !/*js         33*/
      do 60 n=1,maxv                                                            !/*js         34*/
      if(ivdis(n).ne.0)go to 60                                                 !/*js         35*/
      if(n.eq.5.and.nsr.ne.6)write(iuo,40)ht                                    !/*js         36*/
   40 format(4x,3hdir,6x,a12)                                                   !/*js         37*/
      if(dbcn(26).ne.0..and.srv(1,n).eq.0..and.n.eq.maxv)go to 60               !/*jsx         8*/
      if(nsr.ne.6.and.n.ne.5.or.nsr.eq.6.and.hbln(n,kr).ne.' ')                 !/*js4a        4*/
     & write(iuo,50)hbln(n,kr),(srv(i,n),i=1,nvs(n))                            !/*js         40*/
   50 format(4x,a3,1pe18.4,2e13.4)                                              !/*js         41*/
   60 continue                                                                  !/*js         42*/
!                                                                               !/*js         43*/
!        convert distribution numbers from problem names to indexes.            !/*js         44*/
   70 if(ivdis(1).ge.100000)ivdis(1)=0                                          !/*js4a        5*/
      do 100 n=1,maxv                                                           !/*jsx         9*/
      if(ivdis(n).eq.0)go to 100                                                !/*js         46*/
      do 80 j=1,msd                                                             !/*js         47*/
   80 if(ivdis(n).eq.k(1,j))go to 90                                            !/*js         48*/
      call erprnt(1,1,1,ivdis(n),0,0,0,0,                                       !/*js         49*/
     & '12hdistribution,i4,24h for '//hbln(n,kr)//' does not exist.')           !/*js         50*/
   90 ivdis(n)=j                                                                !/*js         51*/
  100 continue                                                                  !/*js         52*/
!                                                                               !/*jsxc        1*/
!        define imt for sdef and ssr transformations.                           !/*jsxc        2*/
      imt=1                                                                     !/*jsxc        3*/
      if (nsr.eq.6) then                                                        !/*jsxc        4*/
        if(njsx(1).gt.njsr(1)) then                                             !/*jsxc        5*/
          imt=njsx(1)/njsr(1)                                                   !/*jsxc        6*/
        elseif(ivdis(5).gt.0) then                                              !/*jsxc        7*/
          if(ksd(lksd+4,ivdis(5)).gt.0) imt=ksd(lksd+4,ivdis(5))                !/*jsxc        8*/
        endif                                                                   !/*jsxc        9*/
      else                                                                      !/*jsxc       10*/
        if(ivdis(20).gt.0) then                                                 !/*jsxc       11*/
          if(ksd(lksd+4,ivdis(20)).gt.0) imt=ksd(lksd+4,ivdis(20))              !/*jsxc       12*/
        endif                                                                   !/*jsxc       13*/
      endif                                                                     !/*jsxc       14*/
!                                                                               !/*jsxc       15*/
      do 140 i=1,msd                                                            !/*js         53*/
      do 130 m=1+k(8,i),k(4,i)*k(6,i),1+k(8,i)                                  !/*js         54*/
      if(spf(k(13,i)+1,m).eq.0.)go to 130                                       !/*js         55*/
      do 110 j=1,msd                                                            !/*js         56*/
  110 if(spf(k(13,i)+1,m).eq.k(1,j))go to 120                                   !/*js         57*/
      call erprnt(1,1,2,int(spf(k(13,i)+1,m)),k(1,i),0,0,0,                     !/*js         58*/
     & '12hdistribution,i4,12h on si or ds,i3,21h card does not exist.')        !/*js         59*/
  120 spf(k(13,i)+1,m)=j                                                        !/*js         60*/
  130 continue                                                                  !/*js         61*/
      if(nsr.eq.0)ksd(lksd+21,i)=1                                              !/*js4c        1*/
  140 ksd(lksd+19,i)=max(0,k(19,i))                                             !/*js         62*/
      do 170 i=lkcp+1,lkcp+mkcp                                                 !/*js         63*/
      if(kcp(i).lt.100000.or.kcp(i).gt.1000000)go to 170                        !/*js         64*/
      do 150 j=1,msd                                                            !/*js         65*/
  150 if(kcp(i)-100000.eq.k(1,j))go to 160                                      !/*js         66*/
      call erprnt(1,1,1,kcp(i)-100000,0,0,0,0,                                  !/*js         67*/
     & '12hdistribution,i4,24h for cel does not exist.')                        !/*js         68*/
  160 kcp(i)=j+100000                                                           !/*js         69*/
      ksd(lksd+12,j)=1                                                          !/*js         70*/
      ksd(lksd+21,j)=1                                                          !/*js         71*/
  170 continue                                                                  !/*js         72*/
!                                                                               !/*js         73*/
!        identify the source variable of each probability distribution.         !/*js         74*/
!        work recursively through any s-option trees encountered.               !/*js         75*/
      do 240 n=1,maxv                                                           !/*jsx        10*/
      i=ivdis(n)                                                                !/*js         77*/
      if(i.eq.0)go to 240                                                       !/*js         78*/
      l=1                                                                       !/*js         79*/
  180 if(k(12,i).ne.0.and.k(12,i).ne.n)                                         !/*js         80*/
     & call erprnt(1,1,1,k(1,i),0,0,0,0,                                        !/*js         81*/
     & '12hdistribution,i4,38h is for more than one source variable.')          !/*js         82*/
      if(k(12,i).ne.0)go to 230                                                 !/*js         83*/
      ksd(lksd+12,i)=n                                                          !/*js         84*/
      ksd(lksd+21,i)=nvs(n)+k(9,i)                                              !/*js         85*/
      if(k(6,i)+k(11,i).ne.0)ksd(lksd+21,i)=1+k(8,i)                            !/*js         86*/
      if((k(4,i)*k(20,i).ne.0.or.k(4,i)*k(6,i)*(1-k(7,i)).ne.0).and.            !/*js4a        7*/
     & k(4,i).ne.k(20,i)*k(21,i))call erprnt(1,1,1,k(1,i),0,0,0,0,              !/*js4a        8*/
     & '47hnon-conforming si, sp, sb cards in distribution,i4')                 !/*js         89*/
      ksd(lksd+4,i)=k(4,i)/k(21,i)                                              !/*js         90*/
      if(k(19,i).ne.0.and.nvs(n).ne.1)call erprnt(1,1,1,k(1,i),0,0,0,0,         !/*js         91*/
     & '17ha option in dist.,i4,23h of vector variable '//hbln(n,1))            !/*js         92*/
      if(k(6,i).eq.0)go to 230                                                  !/*js         93*/
      m=k(21,i)                                                                 !/*js         94*/
  190 if(spf(k(13,i)+1,m).eq.0.)go to 220                                       !/*js         95*/
      if(l.le.20)go to 200                                                      !/*js         96*/
      call erprnt(1,1,1,k(1,i),0,0,0,0,                                         !/*js         97*/
     & '50htoo-long chain of s options to source distribution,i4')              !/*js         98*/
      go to 240                                                                 !/*js         99*/
  200 js(l)=i                                                                   !/*js        100*/
      ms(l)=m                                                                   !/*js        101*/
      l=l+1                                                                     !/*js        102*/
      i=spf(k(13,i)+1,m)                                                        !/*js        103*/
      go to 180                                                                 !/*js        104*/
  210 l=l-1                                                                     !/*js        105*/
      i=js(l)                                                                   !/*js        106*/
      m=ms(l)                                                                   !/*js        107*/
  220 m=m+k(21,i)                                                               !/*js        108*/
      if(m.le.k(4,i)*k(21,i))go to 190                                          !/*js        109*/
  230 if(l.gt.1)go to 210                                                       !/*js        110*/
  240 continue                                                                  !/*js        111*/
!                                                                               !/*js        112*/
!        check for errors in dependent distributions.                           !/*js        113*/
      do 250 n=1,maxv                                                           !/*jsx        11*/
      if(ivdis(n).eq.0)go to 250                                                !/*js        115*/
      if(ivdd(n).eq.n.neqv.k(7,ivdis(n)).eq.0)                                  !/*js        116*/
     & call erprnt(1,1,1,ivdis(n),0,0,0,0,                                      !/*js        117*/
     & '12hdistribution,i4,27h for '//hbln(n,kr)//' is the wrong kind.')        !/*js        118*/
      if(ivdd(n).eq.n)go to 250                                                 !/*js        119*/
      jd=ivdis(n)                                                               !/*js        120*/
      ji=ivdis(ivdd(n))                                                         !/*js        121*/
      if(ivdd(ivdd(n)).ne.ivdd(n))                                              !/*js        122*/
     & call erprnt(1,1,2,k(1,jd),k(1,ji),0,0,0,                                 !/*js        123*/
     & '12hsource dist.,i4,34h depends on dependent distribution,i4')           !/*js        124*/
      if(k(4,ji)+k(8,jd).eq.0)call erprnt(1,1,2,k(1,jd),k(1,ji),0,0,0,          !/*js        125*/
     & '5hdist.,i4,41h with no q option depends on distribution,i4')            !/*js        126*/
      if(k(8,jd).ne.0.and.nvs(k(12,ji)).ne.1)call erprnt                        !/*js        127*/
     & (1,1,2,k(1,jd),k(1,ji),0,0,0,                                            !/*js        128*/
     & '5hdist.,i4,38h with q option depends on vector dist.,i4')               !/*js        129*/
      if(k(9,jd).ne.0.and.k(5,ji).eq.0)                                         !/*js        130*/
     & call erprnt(1,1,2,k(1,jd),k(1,ji),0,0,0,                                 !/*js        131*/
     & '5hdist.,i4,37h with t option depends on cont. dist.,i4')                !/*js        132*/
      if(k(4,jd)+k(5,jd).ne.k(4,ji)+k(5,ji).and.k(4,ji).ne.0.and.               !/*js        133*/
     & k(8,jd)+k(9,jd).eq.0)call erprnt(1,1,1,k(1,jd),0,0,0,0,                  !/*js        134*/
     & '22hdependent distribution,i4,21h is the wrong length.')                 !/*js        135*/
  250 continue                                                                  !/*js        136*/
      do 260 i=1,msd                                                            !/*js        137*/
      if(k(12,i)*k(7,i).eq.0)go to 260                                          !/*js        138*/
      if(i.ne.ivdis(k(12,i)))call erprnt(1,1,1,k(1,i),0,0,0,0,                  !/*js        139*/
     & '22hdependent distribution,i4,20h is used improperly.')                  !/*js        140*/
  260 continue                                                                  !/*js        141*/
!                                                                               !/*js        142*/
!        normalize lengths of axs and vec value sets.                           !/*js        143*/
      do 270 i=12,13                                                            !/*js        144*/
      t1=sqrt(srv(1,i)**2+srv(2,i)**2+srv(3,i)**2)                              !/*js        145*/
      if(t1.eq.0..and.idefv(i).ne.0.and.ivdis(i).eq.0)call erprnt               !/*js        146*/
     & (1,1,0,0,0,0,0,0,'22hlength of '//hbln(i,kr)//' is zero.')               !/*js        147*/
      do 270 j=1,3                                                              !/*js        148*/
  270 if(t1.ne.0.)srv(j,i)=srv(j,i)/t1                                          !/*js        149*/
      do 290 ir=1,msd                                                           !/*js        150*/
      if(k(12,ir).ne.12.and.k(12,ir).ne.13.or.k(6,ir).ne.0)go to 290            !/*js        151*/
      l=k(13,ir)+1                                                              !/*js        152*/
      do 280 i=k(9,ir),k(21,ir)*k(4,ir)-1,k(21,ir)                              !/*js        153*/
      t1=sqrt(spf(l,i+1)**2+spf(l,i+2)**2+spf(l,i+3)**2)                        !/*js        154*/
      do 280 j=1,3                                                              !/*js        155*/
  280 if(t1.ne.0.)spf(l,i+j)=spf(l,i+j)/t1                                      !/*js        156*/
  290 continue                                                                  !/*js        157*/
!                                                                               !/*js        158*/
!        check values of dir distributions.                                     !/*js        159*/
      if(nsr.eq.6)go to 330                                                     !/*js        160*/
      if(idefv(5).ne.0.and.ivdis(5).eq.0.and.idefv(16).eq.0.and.                !/*js        161*/
     & ndet(nips)+ndx(nips).gt.0)call erprnt(1,1,0,0,0,0,0,0,                   !/*cor4-2    120*/
     & '59hsdef requires ara if dir=constant with detectors or dxtran.')        !/*js        163*/
      if(idefv(5).ne.0.and.ivdis(5).eq.0.and.ivdd(5).eq.5.and.                  !/*js4b        1*/
     & ndx(nips).gt.0)call erprnt(1,2,0,0,0,0,0,0,                              !/*js4b        2*/
     & '49hdxtran usually wrong for monodirectional sources.')                  !/*js4b        3*/
      do 320 i=1,msd                                                            !/*js        164*/
      if(k(12,i).ne.5.or.k(6,i)+k(11,i).ne.0.or.k(4,i).eq.0)go to 320           !/*js        165*/
      do 300 j=k(21,i),k(4,i)*k(21,i),k(21,i)                                   !/*js        166*/
  300 if(abs(spf(k(13,i)+1,j)).gt.1.)go to 310                                  !/*js        167*/
      go to 320                                                                 !/*js        168*/
  310 call erprnt(1,1,1,k(1,i),0,0,0,0,                                         !/*js        169*/
     & '22hvalues of distribution,i4,27h for dir are bigger than 1.')           !/*js        170*/
  320 continue                                                                  !/*js        171*/
!                                                                               !/*js        172*/
!        normalize and print the source probability distributions.              !/*js        173*/
  330 do 350 irs=1,msd                                                          !/*js        174*/
      if(k(2,irs)*k(19,irs).ne.0)call erprnt(1,1,1,k(1,irs),0,0,0,0,            !/*js        175*/
     & '12hdistribution,i4,38h has both the a option and a function.')          !/*js        176*/
      if(k(2,irs)*k(6,irs).ne.0)call erprnt(1,1,1,k(1,irs),0,0,0,0,             !/*js        177*/
     & '12hdistribution,i4,38h has both the s option and a function.')          !/*js        178*/
      if(k(10,irs).ne.0.and.k(12,irs).ne.1)                                     !/*js        179*/
     & call erprnt(1,1,1,k(1,irs),0,0,0,0,                                      !/*js        180*/
     & '40hv option on non-cell source distribution,i4')                        !/*js        181*/
      if(k(12,irs).eq.0.and.nsr.ne.0)call erprnt(1,2,1,k(1,irs),0,0,0,0,        !/*js4c        2*/
     & '19hsource distribution,i4,29h is not used in this problem.')            !/*js        183*/
      if(k(7,irs)+k(10,irs).ne.0.or.(k(12,irs).eq.0.and.nsr.ne.0).or.           !/*js4c        3*/
     & k(2,irs)*(k(6,irs)+k(19,irs)).ne.0)go to 350                             !/*js        185*/
      if(k(2,irs).ne.0)go to 340                                                !/*js        186*/
      if(k(19,irs).eq.0)call normh(irs)                                         !/*js        187*/
      if(k(19,irs).ne.0)call norma(irs)                                         !/*js        188*/
      if(k(2,irs).eq.0)go to 350                                                !/*js        189*/
  340 call sprob                                                                !/*js        190*/
  350 continue                                                                  !/*js        191*/
      do 352 i=1,msd                                                            !/*js4c        4*/
      if(i.gt.1)ksd(lksd+16,i)=k(16,i-1)+2*k(4,i-1)                             !/*js        193*/
  352 if(k(2,i)+k(10,i).eq.0.and.k(12,i)*ink(10).ne.0)call prsdst(i)            !/*js4c        5*/
      if(nsr.eq.6)return                                                        !/*js        195*/
!                                                                               !/*js        196*/
!        convert all cell and surface problem names to program names.           !/*js        197*/
      if(srv(1,1).gt.0.)srv(1,1)=namchg(1,int(srv(1,1)))                        !/*js        198*/
      kp=ivdis(2)+ivdis(6)+ivdis(7)+ivdis(8)+ivdis(9)+ivdis(10)+                !/*js4c        6*/
     & ivdis(11)+ivdis(12)+srv(1,2)+srv(1,7)+srv(1,8)+srv(1,9)+                 !/*js4c        7*/
     & srv(1,10)+srv(1,11)+srv(1,12)                                            !/*js4c        8*/
      if(kp.ne.0)go to 354                                                      !/*js4c        9*/
      xxx=srv(1,6)                                                              !/*js4c       10*/
      yyy=srv(2,6)                                                              !/*js4c       11*/
      zzz=srv(3,6)                                                              !/*js4c       12*/
      if(srv(1,1).le.0.)go to 356                                               !/*js4c       13*/
      call chkcel(nint(srv(1,1)),2,kq)                                          !/*js4c       14*/
      if(kq.ne.0)call erprnt(1,1,1,ncl(lncl+nint(srv(1,1))),0,0,0,0,            !/*js4c       15*/
     & '43hpos point source is not located in sdef cel,i5,1h.')                 !/*js4c       16*/
  354 if(srv(1,1).le.0..or.junf.eq.0)go to 356                                  !/*js4c       17*/
      if(jun(ljun+int(srv(1,1))).ne.0)call erprnt(1,1,0,0,0,0,0,0,              !/*js4a       10*/
     & '51hsdef cel entry requires repeated structures format.')                !/*js4a       11*/
  356 jq=srv(1,2)                                                               !/*js4c       18*/
      if(jq.lt.0)go to 358                                                      !/*js4c       19*/
      if(jq.eq.0)go to 362                                                      !/*js4c       20*/
      j1=namchg(2,jq)                                                           !/*js4c       21*/
      if(j1.le.0)go to 358                                                      !/*js4c       22*/
!        check if source surface is facet of a macrobody.                       !/*js4c       23*/
      if(ksm(lksm+j1).eq.0)go to 360                                            !/*js4c       24*/
      iq=0                                                                      !/*js4c       25*/
      if(srv(1,2)-jq.ge.0.099)iq=(srv(1,2)-jq+.001)*10.                         !/*js4c       26*/
      if(iq.le.0)go to 358                                                      !/*js4c       27*/
      j1=j1+iq                                                                  !/*js4c       28*/
      go to 360                                                                 !/*js4c       29*/
  358 call erprnt(2,1,0,0,0,0,0,0,                                              !/*js4c       30*/
     & '31hsdef surface not valid surface.')                                    !/*js4c       31*/
      go to 362                                                                 !/*js4c       32*/
  360 srv(1,2)=j1                                                               !/*js4c       33*/
      call srcsrf(j1)                                                           !/*js4c       34*/
  362 srv(1,14)=namchg(1,nint(srv(1,14)))                                       !/*js4c       35*/
      kf=0                                                                      !/*js4a       13*/
      do 410 ir=1,msd                                                           !/*js        202*/
      if(k(11,ir).ne.0.or.k(12,ir).eq.0)go to 410                               !/*js        203*/
      ks=k(12,ir)                                                               !/*js        204*/
      if(ks.eq.14)ks=1                                                          !/*js        205*/
      if(ks.le.2.and.k(5,ir).eq.0)call erprnt(1,1,1,k(1,ir),0,0,0,0,            !/*js        206*/
     & '20h'//hcs(ks)//' distribution,i4,15h is continuous.')                   !/*js        207*/
!                                                                               !/*js        208*/
!        cell or surface names as values.                                       !/*js        209*/
      if(ks.gt.2.or.k(5,ir).eq.0.or.k(6,ir).ne.0)go to 390                      !/*js        210*/
      do 380 i=k(21,ir),k(4,ir)*k(21,ir),k(21,ir)                               !/*js        211*/
      n=spf(k(13,ir)+1,i)                                                       !/*js        212*/
      if(n.gt.0)go to 364                                                       !/*js4c       36*/
      if(nlev.gt.1.and.ks.eq.1)go to 380                                        !/*js4c       37*/
      if(n.eq.0)call erprnt(1,2,2,n,k(1,ir),0,0,0,'7h'//hcs(ks)//               !/*js4c       38*/
     & ',i6,16h of distribution,i4,9h is zero.')                                !/*js4c2       1*/
      if(n.lt.0)call erprnt(1,2,2,n,k(1,ir),0,0,0,'7h'//hcs(ks)//               !/*js4c       40*/
     & ',i6,16h of distribution,i4,13h is negative.')                           !/*js4c2       2*/
      go to 380                                                                 !/*js4c       42*/
  364 m=namchg(ks,n)                                                            !/*js4c       43*/
      if(ks.ne.2)go to 370                                                      !/*js4c       44*/
      j1=m                                                                      !/*js4c       45*/
      if(j1.le.0)go to 366                                                      !/*js4c       46*/
!        check if source surface is facet of a macrobody.                       !/*js4c       47*/
      if(ksm(lksm+j1).eq.0)go to 368                                            !/*js4c       48*/
      iq=0                                                                      !/*js4c       49*/
      if(spf(k(13,ir)+1,i)-n.ge.0.099)                                          !/*js4c       50*/
     & iq=(spf(k(13,ir)+1,i)-n+.001)*10.                                        !/*js4c       51*/
      if(iq.le.0)go to 366                                                      !/*js4c       52*/
      j1=j1+iq                                                                  !/*js4c       53*/
      go to 368                                                                 !/*js4c       54*/
  366 call erprnt(2,1,0,0,0,0,0,0,                                              !/*js4c       55*/
     & '31hsdef surface not valid surface.')                                    !/*js4c       56*/
      m=0                                                                       !/*js4c       57*/
      go to 372                                                                 !/*js4c       58*/
  368 spf(k(13,ir)+1,i)=j1                                                      !/*js4c       59*/
      m=j1                                                                      !/*js4c       60*/
      go to 374                                                                 !/*js4c       61*/
  370 spf(k(13,ir)+1,i)=m                                                       !/*js4c       62*/
      if(m.ne.0)go to 374                                                       !/*js4c       63*/
  372 call erprnt(1,1,2,n,k(1,ir),0,0,0,'7h'//hcs(ks)//                         !/*js4c       64*/
     & ',i6,16h of distribution,i4,16h is not defined.')                        !/*js        218*/
      go to 380                                                                 !/*js        219*/
  374 if(k(12,ir).eq.2)call srcsrf(m)                                           !/*js4c       65*/
      if(nlev.gt.1.or.kp.ne.0.or.ks.ne.1)go to 376                              !/*js4c       66*/
      call chkcel(m,2,kq)                                                       !/*js4c       67*/
      if(kq.ne.0)call erprnt(1,1,2,n,k(1,ir),0,0,0,                             !/*js4c       68*/
     & '26hpos pt src not in sdef cel,i6,16h of distribution,i4,1h.')           !/*js4c       69*/
  376 if(kf.ne.0.or.k(12,ir).ne.1.or.junf.eq.0)go to 380                        !/*js4c       70*/
      if(srv(1,1).lt.0..or.jun(ljun+m).eq.0)go to 380                           !/*js4a       15*/
      call erprnt(1,1,0,0,0,0,0,0,                                              !/*js4a       16*/
     & '52hsdef cel entries require repeated structures format.')               !/*js4a       17*/
      kf=1                                                                      !/*js4a       18*/
  380 continue                                                                  !/*js        221*/
!                                                                               !/*js        222*/
!        cell or surface names as tags.                                         !/*js        223*/
  390 ks=ivdd(k(12,ir))                                                         !/*js        224*/
      if(ks.eq.14)ks=1                                                          !/*js        225*/
      if(ks.gt.2.or.k(8,ir)+k(9,ir).eq.0)go to 410                              !/*js        226*/
      do 400 i=1,k(4,ir)*k(21,ir),k(21,ir)                                      !/*js        227*/
      n=spf(k(13,ir)+1,i)                                                       !/*js        228*/
      if(ks.ne.2.or.n.eq.0)go to 398                                            !/*js4c       71*/
      j1=namchg(2,n)                                                            !/*js4c       72*/
      if(j1.le.0)go to 394                                                      !/*js4c       73*/
!        check if source surface is facet of a macrobody.                       !/*js4c       74*/
      if(ksm(lksm+j1).eq.0)go to 396                                            !/*js4c       75*/
      iq=0                                                                      !/*js4c       76*/
      if(spf(k(13,ir)+1,i)-n.ge.0.099)                                          !/*js4c       77*/
     & iq=(spf(k(13,ir)+1,i)-n+.001)*10.                                        !/*js4c       78*/
      if(iq.le.0)go to 394                                                      !/*js4c       79*/
      j1=j1+iq                                                                  !/*js4c       80*/
      go to 396                                                                 !/*js4c       81*/
  394 call erprnt(2,1,0,0,0,0,0,0,                                              !/*js4c       82*/
     & '31hsdef surface not valid surface.')                                    !/*js4c       83*/
      spf(k(13,ir)+1,i)=0                                                       !/*js4c       84*/
      go to 400                                                                 !/*js4c       85*/
  396 spf(k(13,ir)+1,i)=j1                                                      !/*js4c       86*/
      go to 400                                                                 !/*js4c       87*/
  398 spf(k(13,ir)+1,i)=namchg(ks,n)                                            !/*js4c       88*/
  400 if(spf(k(13,ir)+1,i).eq.0..and.n.ne.0)call erprnt                         !/*js        230*/
     & (1,1,2,n,k(1,ir),0,0,0,'7h'//hcs(ks)//                                   !/*js        231*/
     & ',i6,16h of distribution,i4,16h is not defined.')                        !/*js        232*/
  410 continue                                                                  !/*js        233*/
      n=-2                                                                      !/*js        234*/
      m=0                                                                       !/*js        235*/
      do 440 i=lkcp+3,lkcp+mkcp                                                 !/*js        236*/
      if(n.ne.-2)go to 420                                                      !/*js        237*/
      l=-1                                                                      !/*js        238*/
      j=i-1                                                                     !/*js        239*/
      n=kcp(j)                                                                  !/*js        240*/
  420 if(kcp(i).eq.1000001)m=1                                                  !/*js        241*/
      if(m.ne.0)go to 430                                                       !/*js        242*/
      l=l+1                                                                     !/*js        243*/
      if(kcp(j-1).eq.-1.and.kcp(i).le.0)kcp(j-1)=l                              !/*js        244*/
      if(n.gt.0.and.abs(kcp(i)).lt.100000)kcp(i)=namchg(1,abs(kcp(i)))          !/*js        245*/
  430 if(kcp(i).eq.1000002)m=0                                                  !/*js        246*/
      if(n.eq.1)kcp(j)=l                                                        !/*js        247*/
      if(n.eq.1.and.kcp(j-1).eq.-1)kcp(j-1)=l                                   !/*js        248*/
  440 n=n-1                                                                     !/*js        249*/
!                                                                               !/*js        250*/
!        set the order of sampling the source variables.                        !/*js        251*/
      do 450 i=1,maxf                                                           !/*js        252*/
      do 450 j=1,maxf                                                           !/*js        253*/
  450 ko(i,j)=0                                                                 !/*js        254*/
      ko(3,6)=1                                                                 !/*js        255*/
      ko(5,2)=1                                                                 !/*js        256*/
      if(idefv(2).ne.0)ko(5,6)=1                                                !/*js        257*/
      ko(6,1)=1                                                                 !/*js        258*/
      ko(6,2)=1                                                                 !/*js        259*/
      ko(6,7)=1                                                                 !/*js        260*/
      ko(6,8)=1                                                                 !/*js        261*/
      ko(6,9)=1                                                                 !/*js        262*/
      ko(6,10)=1                                                                !/*js        263*/
      ko(6,11)=1                                                                !/*js        264*/
      ko(6,12)=1                                                                !/*js        265*/
      ko(6,14)=1                                                                !/*js        266*/
      ko(10,2)=1                                                                !/*js        267*/
      ko(10,12)=1                                                               !/*js        268*/
      do 470 i=1,maxf                                                           !/*js        269*/
      if(ivdd(i).ne.i)ko(ivdd(i),i)=0                                           !/*js        270*/
      mr(i)=1                                                                   !/*js        271*/
      if(idefv(i).ne.0.or.i.eq.3.or.i.eq.4.or.i.eq.6)go to 470                  !/*js        272*/
      mr(i)=0                                                                   !/*js        273*/
      do 460 j=1,maxf                                                           !/*js        274*/
  460 ko(j,i)=0                                                                 !/*js        275*/
  470 if(ivdd(i).ne.i)ko(i,ivdd(i))=1                                           !/*js        276*/
  480 m=0                                                                       !/*js        277*/
      do 500 jy=1,maxf                                                          !/*js        278*/
      i=ij(jy)                                                                  !/*js        279*/
      if(mr(i).eq.0)go to 500                                                   !/*js        280*/
      m=1                                                                       !/*js        281*/
      do 490 j=1,maxf                                                           !/*js        282*/
  490 if(ko(i,j).ne.0)go to 500                                                 !/*js        283*/
      go to 510                                                                 !/*js        284*/
  500 continue                                                                  !/*js        285*/
      if(m.ne.0)call erprnt(1,1,0,0,0,0,0,0,                                    !/*js        286*/
     & '40himpossible source variable dependencies.')                           !/*js        287*/
      go to 530                                                                 !/*js        288*/
  510 nord=nord+1                                                               !/*js        289*/
      ivord(nord)=i                                                             !/*js        290*/
      if(nnpos.eq.0.and.i.ge.6.and.i.le.14)nnpos=nord                           !/*js4a       19*/
      mr(i)=0                                                                   !/*js        291*/
      do 520 j=1,maxf                                                           !/*js        292*/
  520 ko(j,i)=0                                                                 !/*js        293*/
      go to 480                                                                 !/*js        294*/
!                                                                               !/*js        295*/
!        print order of sampling pertinent source variables.                    !/*js        296*/
  530 if(ink(10).ne.0)write(iuo,540)(hbln(ivord(n),1),n=1,nord)                 !/*js        297*/
  540 format(//36h order of sampling source variables./16(1x,a3))               !/*js        298*/
!                                                                               !/*jsx        12*/
!        spontaneous fission source.                                            !/*jsxk        5*/
      if(ink(10).ne.0.and.isfiss.ne.0)                                          !/*jsxk        6*/
     & write(iuo,'(/4x,27hspontaneous fission source.)')                        !/*jsxk        7*/
!                                                                               !/*jsxk        8*/
!        check for missing sdef card surface transformations.                   !/*jsxk        9*/
      do 51 j=1,imt                                                             !/*jsx        13*/
      jt=srv(1,20)                                                              !/*jsx        14*/
      if(imt.gt.1)jt=spf(ksd(lksd+13,ivdis(20))+1,j)                            !/*jsx        15*/
      if(jt.eq.0)go to 51                                                       !/*jsx        16*/
      do 31 i=1,mxtr                                                            !/*jsx        17*/
   31 if(abs(trf(ltrf+1,i)).eq.jt)go to 41                                      !/*jsx        18*/
      call erprnt(1,1,1,jt,0,0,0,0,                                             !/*jsx        19*/
     & '14htransformation,i4,35h for the general source is missing.')           !/*jsx        20*/
   41 if(imt.eq.1)srv(1,20)=i                                                   !/*jsx        21*/
      if(imt.gt.1)spf(ksd(lksd+13,ivdis(20))+1,j)=i                             !/*jsx        22*/
   51 continue                                                                  !/*jsx        23*/
!                                                                               !/*jsx        24*/
      if (imt.le.1) go to 61                                                    !/*jsx        25*/
      ii=ksd(lksd+5,ivdis(20))+ksd(lksd+6,ivdis(20))+ksd(lksd+9,                !/*jsx        26*/
     & ivdis(20))+ksd(lksd+11,ivdis(20))                                        !/*jsx        27*/
      if (ii.eq.1) go to 61                                                     !/*jsx        28*/
      call erprnt(1,1,0,0,0,0,0,0,                                              !/*jsx        29*/
     & '40hwrong SI option for SDEF transformation.')                           !/*jsx        30*/
   61 continue                                                                  !/*jsx        31*/
!                                                                               !/*jsx        32*/
      return                                                                    !/*js        299*/
      end                                                                       !/*js        300*/
