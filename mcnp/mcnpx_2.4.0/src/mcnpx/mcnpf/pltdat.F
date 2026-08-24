!#ident pltdat.F from deck puv4c2                                               !/*puv4c2      1*/
      subroutine pltdat(mm,jv,ip,nv,ie,ar,an,ha)                                !/*puvk        1*/
#ifdef PLOT                                                                     /*!puvx        1*/
!        read or modify cell data card entries.                                 !/*puv4c2      4*/
!        mm = 1  put card name in ha                                            !/*puv4c2      5*/
!        mm = 2  get value, ar, for card hc(jv), particle type ip               !/*puv4c2      6*/
!        mm = 3  replace value, ar, for card hc(jv), particle type ip           !/*puv4c2      7*/
!        mm = 4  get max,min values, ar,an for item jv and particle ip          !/*puvk        2*/
!        mm =-2  use program name, not problem name, for cell, material.        !/*puvk        3*/
!        nv = card number, ie, wwn9:p where nv=9, ip=2, jv=11                   !/*puv4c2      8*/
      use global_data                                                           !/*puvx        2*/
      use plot_data                                                             !/*puvxx       1*/
!                                                                               !/*puvx        3*/
      implicit real(kindr) (a-h,o-z)                                            !/*puvx        4*/
      implicit integer(kindi) (i-n)                                             !/*puvx        5*/
!                                                                               !/*puvx        6*/
      dimension np(mclb),nn(mclb)                                               !/*puv4c2     11*/
      character ha*80,hb*10,hc(mclb)*4                                          !/*puv4c2     12*/
      data hc/'cel','imp','rho','den','vol','fcl','mas','pwt','mat',            !/*puv4c2     13*/
     & 'tmp','wwn','ext','pd','dxc','u','lat','fill','nonu','pac'/              !/*puv4c2     14*/
      data np/0,1,3*0,1,4*0,1,1,0,1,4*0,1/,nn/9*0,1,1,0,1,1,4*0,1/              !/*puv4c2     15*/
!                                                                               !/*puv4c2     16*/
      go to(10,100,400,100)abs(mm)                                              !/*puvk        4*/
!                                                                               !/*puv4c2     18*/
! ************ mm=1  get full card name ********************************        !/*puv4c2     19*/
!                                                                               !/*puv4c2     20*/
! >>>>>  mm=1  get full name of card hc(jv) from jv, ip, nv                     !/*puv4c2     21*/
   10 ar=0.                                                                     !/*puv4c2     22*/
      ha=hc(jv)                                                                 !/*puv4c2     23*/
      l=leng(hc(jv))                                                            !/*puv4c2     24*/
      if(nn(jv).ne.0)call ishort(nv,hb,n)                                       !/*puv4c2     25*/
      n=min(n,4)                                                                !/*puv4c2     26*/
      if(nn(jv).ne.0)ha(l+1:l+n)=hb(1:n)                                        !/*puv4c2     27*/
      if(nn(jv).ne.0)l=l+n                                                      !/*puv4c2     28*/
      ip=max(1,min(mipt,ip))                                                    !/*puv4c2     29*/
      if(kpt(ip).eq.0)ip=nips                                                   !/*puv4c2     30*/
      if(np(jv).ne.0)ha(l+1:l+2)=':'//hip(ip:ip)                                !/*puv4c2     31*/
      if(np(jv).ne.0)l=l+2                                                      !/*puv4c2     32*/
      hb=ha                                                                     !/*puv4c2     33*/
      ha=' '                                                                    !/*puv4c2     34*/
      ha(10-l+1:10)=hb(1:l)                                                     !/*puv4c2     35*/
      return                                                                    !/*puv4c2     36*/
!                                                                               !/*puv4c2     37*/
! ************ mm=2,4 get value of card ********************************        !/*puvk        5*/
!                                                                               !/*puvk        6*/
! >>>>>  mm=2  put the value of card/label command hc(jv) into ar.              !/*puvk        7*/
! >>>>>  mm=-2 use program name, not problem name, for cell, material.          !/*puvk        8*/
! >>>>>  mm=4  get max,min values, ar,an of card/label command hc(jv).          !/*puvk        9*/
  100 if(mm.eq.4.and.iap.eq.0)go to 360                                         !/*puvk       10*/
      am=zero                                                                   !/*puvk       11*/
      an=huge                                                                   !/*puvk       12*/
      do 350 ic=1,mxa                                                           !/*puvk       13*/
      ar=zero                                                                   !/*puvk       14*/
      if(mm.eq.4)iap=ic                                                         !/*puvk       15*/
      if(junf.eq.0.and.jv.ge.15.and.jv.le.17)go to 330                          !/*puvk       16*/
      select case (hc(jv))                                                      !/*puvk       17*/
      case ('cel')                                                              !/*puvk       18*/
         ar=ncl(lncl+iap)                                                       !/*puvk       19*/
         if(mm.ne.2)ar=iap                                                      !/*puvk       20*/
      case ('imp')                                                              !/*puvk       21*/
         ar=fim(lfim+ip,iap)                                                    !/*puvk       22*/
      case ('rho')                                                              !/*puvk       23*/
         ar=rho(lrho+iap)                                                       !/*puvk       24*/
      case ('den')                                                              !/*puvk       25*/
         ar=den(lden+iap)                                                       !/*puvk       26*/
      case ('vol')                                                              !/*puvk       27*/
         ar=vol(lvol+iap)                                                       !/*puvk       28*/
      case ('fcl')                                                              !/*puvk       29*/
         ar=for(lfor+ip,iap)                                                    !/*puvk       30*/
      case ('mas')                                                              !/*puvk       31*/
         ar=den(lden+iap)*vol(lvol+iap)                                         !/*puvk       32*/
      case ('pwt')                                                              !/*puvk       33*/
         if(kpt(1)*kpt(2).eq.0)go to 330                                        !/*puvk       34*/
         ar=gwt(lgwt+iap)                                                       !/*puvk       35*/
      case ('mat')                                                              !/*puvk       36*/
         ar=mat(lmat+iap)                                                       !/*puvk       37*/
         if(mm.eq.2.and.ar.ne.0)ar=nmt(lnmt+mat(lmat+iap))                      !/*puvk       38*/
      case ('tmp')                                                              !/*puvk       39*/
         if(nv.gt.mxt)go to 330                                                 !/*puvk       40*/
         ar=tmp(ltmp+nv+(iap-1)*mxt)                                            !/*puvk       41*/
      case ('wwn')                                                              !/*puvk       42*/
         if (nww(ip).eq.0) then                                                 !/*puvk       43*/
            if(abs(mm).eq.2)go to 360                                           !/*puvk       44*/
            go to 350                                                           !/*puvk       45*/
         endif                                                                  !/*puvk       46*/
         nm=max(1,min(nww(ip),nv))                                              !/*puvk       47*/
         if (wwp(ip,5).ge.0.) then                                              !/*puvk       48*/
            ar=wwf(lwwf+iap+mxa*(nm-1+mww(ip)))                                 !/*puvk       49*/
            go to 320                                                           !/*puvk       50*/
         elseif(mm.ne.4)then                                                    !/*puvk       51*/
!         iap = mesh index from celnbr.                                         !/*puvk       52*/
            ar=wwf(lwwf+iap+nwwm*(nm-1+mww(ip)))                                !/*puvk       53*/
         else                                                                   !/*puvk       54*/
            do n=1,nwwm                                                         !/*puvk       55*/
               ar=wwf(lwwf+n+nwwm*(nm-1+mww(ip)))                               !/*puvk       56*/
               am=max(am,ar)                                                    !/*puvk       57*/
               if(ar.gt.zero)an=min(an,ar)                                      !/*puvk       58*/
            enddo                                                               !/*puvk       59*/
            ar=am                                                               !/*puvk       60*/
         endif                                                                  !/*puvk       61*/
         return                                                                 !/*puvk       62*/
      case ('ext')                                                              !/*puvk       63*/
         ar=qax(lqax+ip,iap)                                                    !/*puvk       64*/
      case ('pd')                                                               !/*puvk       65*/
         if(mod(nv,5).ne.0)go to 330                                            !/*puvk       66*/
         do 240 n=1,ntal                                                        !/*puvk       67*/
  240    if(jptal(ljpt+1,n).eq.nv)go to 250                                     !/*puvk       68*/
         go to 330                                                              !/*puvk       69*/
  250    if(iptal(lipt+2,2,n).eq.0)go to 330                                    !/*puvk       70*/
         ar=tds(iptal(lipt+2,2,n)+iap)                                          !/*puvk       71*/
      case ('dxc')                                                              !/*puvk       72*/
         if(ndx(1)+ndx(2).eq.0.or.ip.gt.2)go to 330                             !/*puvk       73*/
         ar=dxcp(ldxp,ip,iap)                                                   !/*puvk       74*/
      case ('u')                                                                !/*puvk       75*/
         ar=jun(ljun+iap)                                                       !/*puvk       76*/
      case ('lat')                                                              !/*puvk       77*/
         ar=lat(llat+1,iap)                                                     !/*puvk       78*/
      case ('fill')                                                             !/*puvk       79*/
         ar=mfl(lmfl+1,iap)                                                     !/*puvk       80*/
      case ('nonu')                                                             !/*puvk       81*/
         ar=lfcl(llfc+iap)                                                      !/*puvk       82*/
      case ('pac')                                                              !/*puvk       83*/
         if(nps.le.0.or.nv.gt.8)go to 330                                       !/*puvk       84*/
         ar=pac(lpac+ip,nv,iap)                                                 !/*puvk       85*/
         if(nv.le.3)go to 320                                                   !/*puvk       86*/
         f=zero                                                                 !/*puvk       87*/
         select case (nv)                                                       !/*puvk       88*/
         case (4)                                                               !/*puvk       89*/
            f=one/max(nps,1)                                                    !/*puvk       90*/
            if(nsr.eq.6.and.nrrs.ge.nrss)f=one/max(np1,1)                       !/*puvk       91*/
            if(nsr.eq.6.and.nrrs.lt.nrss)f=one/max(npsr,1)                      !/*puvk       92*/
         case (5)                                                               !/*puvk       93*/
            if(pac(lpac+ip,9,iap).ne.zero)f=one/pac(lpac+ip,9,iap)              !/*puvk       94*/
         case (6)                                                               !/*puvk       95*/
            if(pac(lpac+ip,10,iap).ne.zero)f=one/pac(lpac+ip,10,iap)            !/*puvk       96*/
         case (7)                                                               !/*puvk       97*/
            if(ar.ne.zero)f=pac(lpac+ip,10,iap)*fim(lfim+ip,iap)/ar**2          !/*puvk       98*/
         case (8)                                                               !/*puvk       99*/
            if(pac(lpac+ip,10,iap).ne.zero)f=one/pac(lpac+ip,10,iap)            !/*puvk      100*/
         end select                                                             !/*puvk      101*/
         ar=ar*f                                                                !/*puvk      102*/
      end select                                                                !/*puvk      103*/
!                                                                               !/*puvk      104*/
!        requested information found.                                           !/*puvk      105*/
  320 if(mm.ne.4)return                                                         !/*puvk      106*/
      am=max(am,ar)                                                             !/*puvk      107*/
      if(ar.gt.zero)an=min(an,ar)                                               !/*puvk      108*/
      go to 350                                                                 !/*puvk      109*/
!                                                                               !/*puvk      110*/
!        requested nonexistent information.                                     !/*puvk      111*/
  330 if (mm.eq.4) go to 350                                                    !/*puvk      112*/
      ie=1                                                                      !/*puvk      113*/
      return                                                                    !/*puvk      114*/
!                                                                               !/*puvk      115*/
  350 continue                                                                  !/*puvk      116*/
      ar=am                                                                     !/*puvk      117*/
      return                                                                    !/*puvk      118*/
!                                                                               !/*puvk      119*/
  360 ie=-1                                                                     !/*puvk      120*/
      ar=0.                                                                     !/*puv4c2    112*/
      return                                                                    !/*puv4c2    113*/
!                                                                               !/*puv4c2    114*/
! ************ mm=3  reset label parameter *****************************        !/*puv4c2    115*/
!                                                                               !/*puv4c2    116*/
! >>>>>  reset values of parameters for label hc(i) to value of ar.             !/*puv4c2    117*/
  400 if(np(jv).ne.0.and.kpt(ip).eq.0)go to 590                                 !/*puv4c2    118*/
      if(jv.lt.1.or.jv.gt.mclb)go to 590                                        !/*puv4c2    119*/
      ie=0                                                                      !/*puv4c2    120*/
      go to(410,420,430,440,450,460,465,470,480,510,520,530,540,570,600,        !/*puv4c2    121*/
     & 600,600,580,600)jv                                                       !/*puv4c2    122*/
  410 ic=nint(ar)                                                               !/*puv4c2    123*/
      ncl(lncl+iap)=ic                                                          !/*puv4c2    124*/
      return                                                                    !/*puv4c2    125*/
  420 fim(lfim+ip,iap)=ar                                                       !/*puv4c2    126*/
      return                                                                    !/*puv4c2    127*/
  430 if(rho(lrho+iap).ne.0.)                                                   !/*puv4c2    128*/
     & den(lden+iap)=ar*den(lden+iap)/rho(lrho+iap)                             !/*puv4c2    129*/
      rho(lrho+iap)=ar                                                          !/*puv4c2    130*/
      return                                                                    !/*puv4c2    131*/
  440 if(den(lden+iap).ne.0.)                                                   !/*puv4c2    132*/
     & rho(lrho+iap)=ar*rho(lrho+iap)/den(lden+iap)                             !/*puv4c2    133*/
      den(lden+iap)=ar                                                          !/*puv4c2    134*/
      return                                                                    !/*puv4c2    135*/
  450 vol(lvol+iap)=ar                                                          !/*puv4c2    136*/
      return                                                                    !/*puv4c2    137*/
  460 for(lfor+ip,iap)=ar                                                       !/*puv4c2    138*/
      return                                                                    !/*puv4c2    139*/
  465 tm=den(lden+iap)*vol(lvol+iap)                                            !/*puv4c2    140*/
      if(tm.gt.0.)den(lden+iap)=den(lden+iap)*ar/tm                             !/*puv4c2    141*/
      if(tm.gt.0.)rho(lrho+iap)=rho(lrho+iap)*ar/tm                             !/*puv4c2    142*/
      return                                                                    !/*puv4c2    143*/
  470 if(kpt(1)*kpt(2).eq.0)go to 590                                           !/*puv4c2    144*/
      gwt(lgwt+iap)=ar                                                          !/*puv4c2    145*/
      return                                                                    !/*puv4c2    146*/
  480 do 490 m=1,nmat                                                           !/*puv4c2    147*/
  490 if(nmt(lnmt+m).eq.nint(ar))go to 500                                      !/*puv4c2    148*/
      go to 590                                                                 !/*puv4c2    149*/
  500 mat(lmat+iap)=m                                                           !/*puv4c2    150*/
      return                                                                    !/*puv4c2    151*/
  510 if(nv.gt.mxt)go to 590                                                    !/*puv4c2    152*/
      tmp(ltmp+nv+(iap-1)*mxt)=ar                                               !/*puv4c2    153*/
      return                                                                    !/*puv4c2    154*/
  520 if(nww(ip).eq.0)go to 590                                                 !/*puv4c2    155*/
      nv=max(1,min(nww(ip),nv))                                                 !/*puv4c2    156*/
      if(wwp(ip,5).lt.0.)go to 525                                              !/*puv4c2    157*/
      wwf(lwwf+iap+mxa*(nv-1+mww(ip)))=ar                                       !/*puv4c2    158*/
      return                                                                    !/*puv4c2    159*/
  525 ww=wwval(ip,iap,nv,0,jx)                                                  !/*puv4c2    160*/
      if(jx.eq.0.)go to 590                                                     !/*puv4c2    161*/
      wwf(lwwf+jx+nwwm*(nv-1+mww(ip)))=ar                                       !/*puv4c2    162*/
      return                                                                    !/*puv4c2    163*/
  530 qax(lqax+ip,iap)=ar                                                       !/*puv4c2    164*/
      return                                                                    !/*puv4c2    165*/
  540 if(mod(nv,5).ne.0)go to 590                                               !/*puv4c2    166*/
      do 550 n=1,ntal                                                           !/*puv4c2    167*/
  550 if(jptal(ljpt+1,n).eq.nv)go to 560                                        !/*puv4c2    168*/
      go to 590                                                                 !/*puv4c2    169*/
  560 if(iptal(lipt+2,2,n).eq.0)go to 590                                       !/*puv4c2    170*/
      tds(iptal(lipt+2,2,n)+iap)=ar                                             !/*puv4c2    171*/
      return                                                                    !/*puv4c2    172*/
  570 if(ndx(1)+ndx(2).eq.0.or.ip.gt.2)go to 590                                !/*puv4c2    173*/
      dxcp(ldxp,ip,iap)=ar                                                      !/*puv4c2    174*/
      return                                                                    !/*puv4c2    175*/
  580 lfcl(llfc+iap)=nint(ar)-1                                                 !/*puv4c2    176*/
      return                                                                    !/*puv4c2    177*/
!                                                                               !/*puv4c2    178*/
!        error exit.                                                            !/*puv4c2    179*/
  590 ie=1                                                                      !/*puv4c2    180*/
!                                                                               !/*puv4c2    181*/
  600 continue                                                                  !/*puvx        9*/
#endif                                                                          /*!puvx       10*/
      return                                                                    !/*puvx       11*/
      end                                                                       !/*puv4c2    183*/
