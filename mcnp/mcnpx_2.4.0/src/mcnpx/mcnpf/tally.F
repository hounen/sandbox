!#ident tally.F from deck ty                                                    !/*ty          1*/
      subroutine tally(                                                         !/*tyx         1*/
#ifdef MESHTAL                                                                  /*!tyx         2*/
     & qx,mm,                                                                   !/*tyx         3*/
#endif                                                                          /*!tyx         4*/
     & lo,dr)                                                                   !/*tyx         5*/
!        score all tallies for this cell or surface.                            !/*ty          3*/
!        lo is locct(llct+ipt,icl) or locst(llst+ipt,jsu).                      !/*ty          4*/
!        dr = track length in cell or cosine of angle to surface normal.        !/*ty          5*/
!        <<< Update by E. C. Snow 04/10/2000 >>>                                !/*tyx         6*/
      use global_data                                                           !/*tyx         7*/
!                                                                               !/*tyx         8*/
      implicit real(kindr) (a-h,o-z)                                            !/*tyx         9*/
      implicit integer(kindi) (i-n)                                             !/*tyx        10*/
!                                                                               !/*tyx        11*/
      dimension ik(8)                                                           !/*tyx        12*/
!                                                                               !/*ty         14*/
!        return if kcode problem is not settled.                                !/*ty         15*/
      if(kc8.gt.0)return                                                        !/*ty4b        1*/
!                                                                               !/*ty4b        2*/
      li=lipt                                                                   !/*ty         17*/
      lj=ljpt                                                                   !/*ty         18*/
!                                                                               !/*ty4c        1*/
!        warn if tally made by both dxtran and non-dxtran particles.            !/*ty4c        2*/
      if(idx+1.eq.itds(lo-1))go to 5                                            !/*ty4c        3*/
      if(itds(lo-1).lt.0)go to 5                                                !/*ty4c        4*/
      if(ipt.eq.2.or.ipt.eq.1.and.jan.eq.1.and.erg.le.cut_n) then               !/*tyx        13*/
      if(itds(lo-1).ne.0)                                                       !/*ty4c        6*/
     & call errprn(1,-1,1,zero+jptal(lj+1,itds(lo+1)),zero,'tal',' ',           !/*ty4c2       1*/
     & 'tally partly inside and outside of a dxtran sphere.')                   !/*ty4c        8*/
      if(itds(lo-1).ne.0)itds(lo-1)=-1                                          !/*ty4c        9*/
      if(itds(lo-1).eq.0)itds(lo-1)=idx+1                                       !/*ty4c       10*/
      endif                                                                     !/*tyx        14*/
!                                                                               !/*ty         19*/
!        do all tallies that include this cell or surface.                      !/*ty         20*/
    5 tv=tme                                                                    !/*ty4c       11*/
      es=erg                                                                    !/*ty         24*/
      lx=lo+2                                                                   !/*ty         25*/
      do 700 ml=1,itds(lo)                                                      !/*ty         26*/
      ital=itds(lx-1)                                                           !/*ty         27*/
      n1=itds(lx)                                                               !/*ty         28*/
#ifdef MESHTAL                                                                  /*!tyx        15*/
      if(mm.ne.0.and.jptal(lj+2,ital).ne.6)go to 700                            !/*tyx        16*/
      if(jgp.lt.0.and.jptal(lj+2,ital).eq.6) then                               !/*tyx        17*/
         if(ipt.ne.3)go to 700                                                  !/*tyx        18*/
      endif                                                                     !/*tyx        19*/
#else                                                                           /*!tyx        20*/
      if(jgp.lt.0.and.jptal(lj+2,ital).eq.6)go to 700                           !/*cor4-1    173*/
#endif                                                                          /*!tyx        21*/
      tme=tv                                                                    !/*ty4c2       2*/
      ll=lft(llft+3,ital)                                                       !/*ty         31*/
      ta=wgt                                                                    !/*ty         32*/
      if(jptal(lj+2,ital).gt.5.and.rho(lrho+icl).eq.0.)go to 700                !/*ty4c       13*/
      if(iptal(li+3,2,ital).ne.0)ta=dosef(ta)                                   !/*ty         33*/
      ibu=1                                                                     !/*ty         34*/
      if(lft(llft+6,ital)+lft(llft+7,ital).ne.0)ibu=jft(kjft+ital)              !/*ty         35*/
      if(lft(llft+4,ital).ne.0)ibu=jbin(3,ncp+zero)                             !/*ty         36*/
      if(lft(llft+5,ital).eq.0)go to 20                                         !/*ty         37*/
      do 10 ibu=1,iptal(li+3,4,ital)-1                                          !/*ty         38*/
   10 if(tds(iptal(li+3,1,ital)+ibu).eq.ncl(lncl+icl9(npb)))go to 20            !/*ty         39*/
      go to 700                                                                 !/*ty         40*/
   20 if(lft(llft+8,ital).eq.0)go to 40                                         !/*ty         41*/
      if(jgp.le.-2)go to 60                                                     !/*cor4-1    174*/
      l=tds(lft(llft+8,ital)+1)                                                 !/*cor4-1    175*/
      if(l.gt.1)ibu=2                                                           !/*ty         44*/
      if(l.ne.2)ta=-ta                                                          !/*ty         45*/
      go to 60                                                                  !/*ty         46*/
   40 if(lft(llft+9,ital).eq.0)go to 60                                         !/*ty         47*/
      l=jxs(ljxs+1,mgegbt(1))+2*jgm(1)-1                                        !/*ty         48*/
      do 50 ibu=1,iptal(li+3,4,ital)-1                                          !/*ty         49*/
   50 if(abs(tds(iptal(li+3,1,ital)+ibu)-xss(l+jgp)).le..001*xss(l+jgp))        !/*ty         50*/
     & go to 60                                                                 !/*ty         51*/
      go to 700                                                                 !/*ty         52*/
   60 if(ibu.eq.0)go to 700                                                     !/*ty         53*/
      ibs=1                                                                     !/*ty         54*/
      ibc=1                                                                     !/*ty         55*/
      ibe=1                                                                     !/*ty         56*/
      ibt=1                                                                     !/*ty         57*/
      jbd=1                                                                     !/*ty         58*/
!                                                                               !/*ty         59*/
!        gaussian energy broadening.                                            !/*ty         60*/
      erg=es                                                                    !/*ty         61*/
      if(iptal(li+7,1,ital).eq.0)go to 90                                       !/*ty         62*/
      l=lft(llft+2,ital)                                                        !/*ty         63*/
      if(l.eq.0)go to 80                                                        !/*ty         64*/
   70 t1=2.*rang()-1.                                                           !/*ty         65*/
      t2=t1**2+rang()**2                                                        !/*ty         66*/
      if(t2.gt.1.)go to 70                                                      !/*ty         67*/
      erg=erg+.60056120439322*(tds(l+1)+tds(l+2)*sqrt(max(zero,                 !/*ty4b       10*/
     & erg+tds(l+3)*erg**2)))*sqrt(-log(t2)/t2)*t1                              !/*ty4b       11*/
!                                                                               !/*ty         70*/
!        find the energy bin and multiply by the energy bin multiplier.         !/*ty         71*/
   80 ibe=jbin(7,erg)                                                           !/*ty         72*/
      if(ibe.eq.0)go to 700                                                     !/*ty         73*/
      if(iptal(li+7,2,ital).ne.0)ta=ta*tds(iptal(li+7,2,ital)+ibe)              !/*ty         74*/
!                                                                               !/*ty         75*/
!        find the time bin.                                                     !/*ty         76*/
   90 if(iptal(li+8,1,ital).eq.0.or.ll.ne.0)go to 100                           !/*ty         77*/
      ibt=jbin(8,tv)                                                            !/*ty         78*/
      if(ibt.eq.0)go to 700                                                     !/*ty         79*/
!                                                                               !/*ty         80*/
!        set jbd=2 if the tally is flagged.                                     !/*ty         81*/
  100 if(iptal(li+2,1,ital).eq.0)go to 150                                      !/*ty         82*/
      if(iptal(li+2,1,ital).lt.0)go to 120                                      !/*ty         83*/
      l=iptal(li+2,1,ital)                                                      !/*ty         84*/
      do 110 i=1,itds(l)                                                        !/*ty         85*/
  110 if(ifl(kifl+itds(l+i)).ne.0)go to 140                                     !/*ty         86*/
      if(iptal(li+2,2,ital).eq.0)go to 150                                      !/*ty         87*/
  120 l=iptal(li+2,2,ital)                                                      !/*ty         88*/
      do 130 i=1,itds(l)                                                        !/*ty         89*/
  130 if(jfl(kjfl+itds(l+i)).ne.0)go to 140                                     !/*ty         90*/
      go to 150                                                                 !/*ty         91*/
  140 jbd=2                                                                     !/*ty         92*/
!                                                                               !/*ty         93*/
!        set up tally index coefficients.                                       !/*ty         94*/
  150 do 160 i=3,8                                                              !/*ty         95*/
  160 ik(i)=iptal(li+i,5,ital)                                                  !/*ty         96*/
      if(npert.ne.0)call pertc(1,0,0,0,0,zero)                                  !/*ty4c       14*/
!                                                                               !/*ty         97*/
!        branch according to tally type.                                        !/*ty         98*/
      go to(170,180,700,250,700,260,320)jptal(lj+2,ital)                        !/*ty         99*/
!                                                                               !/*ty        100*/
! >>>>>  tally type 1.  find the cosine bin and multiplier.                     !/*ty        101*/
  170 if(iptal(li+6,1,ital).eq.0)go to 190                                      !/*ty        102*/
      l=lft(llft+1,ital)                                                        !/*ty        103*/
      c=dr                                                                      !/*ty        104*/
      if(l.ne.0)c=uuu*tds(l+1)+vvv*tds(l+2)+www*tds(l+3)                        !/*ty        105*/
      ibc=jbin(6,c)                                                             !/*ty        106*/
      if(ibc.eq.0)go to 700                                                     !/*ty        107*/
      if(iptal(li+6,2,ital).ne.0)ta=ta*tds(iptal(li+6,2,ital)+ibc)              !/*ty        108*/
      go to 190                                                                 !/*ty        109*/
!                                                                               !/*ty        110*/
! >>>>>  tally type 2.  divide by the cosine.                                   !/*ty        111*/
  180 c=dr                                                                      !/*tyxa        1*/
      if(iptal(li+6,1,ital).eq.0)go to 185                                      !/*tyxa        2*/
      l=lft(llft+1,ital)                                                        !/*tyxa        3*/
      if(l.ne.0)c=uuu*tds(l+1)+vvv*tds(l+2)+www*tds(l+3)                        !/*tyxa        4*/
      ibc=jbin(6,c)                                                             !/*tyxa        5*/
      if(ibc.eq.0)go to 700                                                     !/*tyxa        6*/
      if(iptal(li+6,2,ital).ne.0)ta=ta*tds(iptal(li+6,2,ital)+ibc)              !/*tyxa        7*/
  185 if(abs(c).ge..1)ta=ta/abs(c)                                              !/*tyxa        8*/
      if(abs(c).lt..1)ta=20.*ta                                                 !/*tyxa        9*/
!                                                                               !/*ty        114*/
!        find the segment bin.  preserve lgc for track.                         !/*ty        115*/
  190 if(iptal(li+4,1,ital).eq.0)go to 240                                      !/*ty        116*/
      i=iptal(li+4,1,ital)                                                      !/*ty        117*/
      if(lca(llca+iap).gt.0)go to 210                                           !/*ty        118*/
      do 200 k=1,abs(lca(llca+iap+1))+lca(llca+iap)                             !/*ty        119*/
  200 iti(k)=lgc(k)                                                             !/*ty        120*/
  210 call chkcel(i,1,j)                                                        !/*ty        121*/
      if(lca(llca+iap).gt.0)go to 230                                           !/*ty        122*/
      do 220 k=1,abs(lca(llca+iap+1))+lca(llca+iap)                             !/*ty        123*/
  220 lgc(k)=iti(k)                                                             !/*ty        124*/
  230 if(j.eq.0)j=abs(lca(llca+i+1))                                            !/*ty        125*/
      ibs=j-abs(lca(llca+i))+1                                                  !/*ty        126*/
!                                                                               !/*ty        127*/
!        multiply by the energy if there is a * on the f card.                  !/*ty        128*/
  240 if(jptal(lj+4,ital).ne.0)ta=ta*erg                                        !/*ty        129*/
      tb=ta                                                                     !/*ty        130*/
      go to 500                                                                 !/*ty        131*/
!                                                                               !/*ty        132*/
! >>>>>  tally type 4.  multiply by the energy if a * is on the f card.         !/*ty        133*/
  250 if(jptal(lj+4,ital).ne.0)ta=ta*erg                                        !/*ty        134*/
      go to 400                                                                 !/*ty4c2       3*/
!                                                                               !/*ty        136*/
! >>>>>  tally type 6.  multiply by the heating.                                !/*ty        137*/
  260 continue                                                                  !/*tyx        22*/
#ifdef MESHTAL                                                                  /*!tyx        23*/
!     QUESTION: Should neutron and proton require non-antiparticle?             !/*tyx        24*/
      if(mm.eq.0.and.(ipt.le.2.or.ipt.eq.9)) then                               !/*tyx        25*/
         if((ipt.eq.1.and.erg.gt.cut_n).or.                                     !/*tyx        26*/
     &    (ipt.eq.9.and.erg.gt.cut_h))go to 700                                 !/*tyx        27*/
         if(ipt.eq.2.and.kpt(3).ne.0.and.                                       !/*tyx        28*/
     &    jptal(lj+4,ital).eq.2)go to 700                                       !/*tyx        29*/
         mk=mat(lmat+icl)                                                       !/*tyx        30*/
         h=heatf(mk,lx)                                                         !/*tyx        31*/
         go to 380                                                              !/*tyx        32*/
!     TEMPORARY expedient: protect from overflow with vsmall.                   !/*tyx        33*/
      elseif(mm.eq.1.and.jptal(lj+4,ital).eq.2.and.dr.gt.vsmall) then           !/*tyx        34*/
         ta=ta*qx/dr                                                            !/*tyx        35*/
         go to 400                                                              !/*tyx        36*/
!     TEMPORARY expedient: protect from overflow with vsmall.                   !/*tyx        37*/
      elseif(mm.eq.2.and.jptal(lj+4,ital).ne.2.and.dr.gt.vsmall) then           !/*tyx        38*/
         ta=ta*qx/dr                                                            !/*tyx        39*/
         go to 400                                                              !/*tyx        40*/
      else                                                                      !/*tyx        41*/
         go to 700                                                              !/*tyx        42*/
      endif                                                                     !/*tyx        43*/
#else                                                                           /*!tyx        44*/
      mk = mat(lmat+icl)                                                        !/*tyx        45*/
      h = heatf(mk,lx)                                                          !/*tyx        46*/
      go to 380                                                                 !/*ty        158*/
#endif                                                                          /*!tyx        47*/
!                                                                               !/*ty        159*/
! >>>>>  tally type 7.  multiply by fission heating.                            !/*ty        160*/
  320 h=0.                                                                      !/*ty        161*/
      mk=mat(lmat+icl)                                                          !/*ty        162*/
      if(mcal.ne.0)go to 360                                                    !/*ty        163*/
      do 350 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                    !/*ty        164*/
      h2=huge                                                                   !/*ty4c       27*/
      i=lme(llme+1,m)                                                           !/*ty        165*/
      if(jxs(ljxs+21,i).eq.0)go to 350                                          !/*ty        166*/
      l=jxs(ljxs+21,i)                                                          !/*ty        167*/
      if(ktc(kktc+1,i).lt.nint(xss(l)))go to 350                                !/*tyx        48*/
      j=l+2+ktc(kktc+1,i)-nint(xss(l))                                          !/*ty4c       28*/
      do 330 k=1,22                                                             !/*ty        170*/
  330 if(nxs(lnxs+2,i).eq.mfiss(k))go to 340                                    !/*ty4a        6*/
  340 if(rtc(krtc+11,i).lt.0.)                                                  !/*ty4c       29*/
     & h2=fme(lfme+m)*qfiss(k)*(xss(j)+rtc(krtc+1,i)*(xss(j+1)-xss(j)))         !/*ty4c       30*/
      if(rtc(krtc+11,i).ge.0.)h2=fme(lfme+m)*qfiss(k)*rtc(krtc+12,i)            !/*ty4c       31*/
      if(npert.ne.0)call pertc(2,mk,m,-6,lx,h2)                                 !/*ty4c       32*/
      h=h+h2                                                                    !/*ty4c       33*/
  350 continue                                                                  !/*ty        173*/
      if(npert.ne.0.and.h2.eq.huge)                                             !/*ty4c       34*/
     & call pertc(2,mk,jmd(ljmd+mk+1)-1,-6,lx,zero)                             !/*ty4c       35*/
      go to 380                                                                 !/*ty        174*/
!        multigroup fission heating.                                            !/*ty        175*/
  360 do 370 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                    !/*ty        176*/
      iex=lme(llme+1,m)                                                         !/*ty        177*/
      h2=getxs(18)*getxs(318)*fme(lfme+m)                                       !/*ty4c       36*/
      if(npert.ne.0)call pertc(2,mk,m,-6,lx,h2)                                 !/*ty4c       37*/
  370 h=h+h2                                                                    !/*ty4c       38*/
  380 ta=ta*h*rho(lrho+icl)                                                     !/*ty        179*/
!                                                                               !/*ty        185*/
!        set up for segment and time bin control.                               !/*ty        186*/
  400 db=dr                                                                     !/*ty        187*/
      da=0.                                                                     !/*ty        188*/
!                                                                               !/*ty        189*/
!        set up to track through the segments.                                  !/*ty        190*/
      if(iptal(li+4,1,ital).eq.0)go to 470                                      !/*ty        191*/
      ig=iptal(li+4,1,ital)                                                     !/*ty        192*/
      call chkcel(ig,3,j)                                                       !/*ty        193*/
      call track(ig)                                                            !/*ty        194*/
      if(kdb.ne.0)return                                                        !/*ty        195*/
      n=iptal(li+4,4,ital)-2                                                    !/*ty        196*/
!                                                                               !/*ty        197*/
!        loop through the segments on the track.                                !/*ty        198*/
  410 db=dr                                                                     !/*ty        199*/
      do 420 ibs=1,n                                                            !/*ty        200*/
  420 if(lgc(ibs).ne.0)go to 430                                                !/*ty        201*/
  430 if(nlt.eq.0)go to 470                                                     !/*ty        202*/
      k=1                                                                       !/*ty        203*/
      do 440 i=2,nlt                                                            !/*ty        204*/
  440 if(dti(i).lt.dti(k))k=i                                                   !/*ty        205*/
      db=min(dr,dti(k))                                                         !/*ty        206*/
      if(db.eq.dr)go to 470                                                     !/*ty        207*/
      dti(k)=huge                                                               !/*ty        208*/
      do 450 j=1,n                                                              !/*ty        209*/
  450 if(abs(lja(llja+abs(lca(llca+ig))+j-1)).eq.iti(k))go to 460               !/*ty        210*/
  460 lgc(j)=1-lgc(j)                                                           !/*ty        211*/
!                                                                               !/*ty        212*/
!        loop through the time bins in the segment.                             !/*ty        213*/
  470 dc=db                                                                     !/*ty        214*/
      if(iptal(li+8,1,ital).eq.0.or.ll.ne.0)go to 490                           !/*ty        215*/
  480 dc=min(db,vel*(tds(iptal(li+8,1,ital)+ibt)-tv))                           !/*ty        216*/
  490 tb=ta*(dc-da)                                                             !/*ty        217*/
      tme=tv+.5*(dc+da)/vel                                                     !/*ty        218*/
!                                                                               !/*ty        219*/
!        do time convolution.                                                   !/*ty        220*/
  500 if(ll.eq.0)go to 520                                                      !/*ty        221*/
      tt=tb                                                                     !/*ty        222*/
      ca=tme+tds(ll+1)                                                          !/*ty        223*/
      cd=tme+tds(ll+2)                                                          !/*ty        224*/
      ibt=1                                                                     !/*ty        225*/
      b=-huge                                                                   !/*ty        226*/
      if(jptal(lj+2,ital).lt.4)go to 510                                        !/*ty        227*/
      dt=(dc-da)/vel                                                            !/*ty        228*/
      tt=ta*vel                                                                 !/*ty        229*/
      cd=cd+.5*dt                                                               !/*ty        230*/
      cb=cd-dt                                                                  !/*ty        231*/
      ca=ca-.5*dt                                                               !/*ty        232*/
      cc=ca+dt                                                                  !/*ty        233*/
  510 a=b                                                                       !/*ty        234*/
      if(a.ge.cd)go to 690                                                      !/*ty        235*/
      b=tds(iptal(li+8,1,ital)+ibt)                                             !/*ty        236*/
      if(b.le.ca)go to 680                                                      !/*ty        237*/
      tb=tt*(min(b,cd)-max(a,ca))                                               !/*ty        238*/
      if(jptal(lj+2,ital).lt.4)tb=tb/(tds(ll+2)-tds(ll+1))                      !/*ty        239*/
      if(jptal(lj+2,ital).ge.4)tb=tb+(tt*.5/(tds(ll+2)-tds(ll+1)))*             !/*ty        240*/
     & (max(zero,a-cc)**2-max(zero,min(b,cd)-cc)**2+                            !/*ty        241*/
     & max(zero,cb-b)**2-max(zero,cb-max(a,ca))**2)                             !/*ty        242*/
!                                                                               !/*ty        243*/
!        set up controls for the totals nest.                                   !/*ty        244*/
  520 jc=iptal(li+6,3,ital)                                                     !/*ty        245*/
      ju=iptal(li+3,3,ital)                                                     !/*ty        246*/
      js=iptal(li+4,3,ital)                                                     !/*ty        247*/
      jm=iptal(li+5,3,ital)                                                     !/*ty        248*/
      je=iptal(li+7,3,ital)                                                     !/*ty        249*/
      jt=iptal(li+8,3,ital)                                                     !/*ty        250*/
!                                                                               !/*ty        251*/
!        multiply by time bin multiplier.  do the multiplier bins.              !/*ty        252*/
      if(iptal(li+8,2,ital).ne.0)tb=tb*tds(iptal(li+8,2,ital)+ibt)              !/*ty        253*/
      kk=0                                                                      !/*ty        254*/
      do 670 km=1,iptal(li+5,4,ital)-1                                          !/*ty        255*/
      tc=tb                                                                     !/*ty        256*/
      lm=iptal(li+5,4,ital)-km                                                  !/*ty        257*/
      if(iptal(li+5,2,ital).eq.0)go to 530                                      !/*ty        258*/
      if(ptbtc.ne.0.)call pertc(1,0,0,0,0,zero)                                 !/*ty4c       39*/
      kk=kk+1                                                                   !/*ty        259*/
      if(iptal(li+5,1,ital).ne.0)call wtmult(tc,kk)                             !/*ty        260*/
      tc=tc*tds(iptal(li+5,2,ital)+kk)                                          !/*ty        261*/
      if(tds(iptal(li+5,2,ital)+kk).lt.0..and.jptal(lj+2,ital).eq.4)            !/*ty        262*/
     & tc=-tc*rho(lrho+icl)                                                     !/*ty        263*/
!                                                                               !/*ty        264*/
!        do the surface or cell bins.                                           !/*ty        265*/
  530 mr=1                                                                      !/*ty4a        8*/
      do 660 ir=1,n1                                                            !/*ty4a        9*/
      if(itds(lx+ir).eq.iptal(li+1,4,ital))go to(660,539)mr                     !/*ty4a       10*/
      lr=itds(iptal(li+1,1,ital)+itds(lx+ir))                                   !/*ty4a       11*/
      lr=lr+itds(lr)+1                                                          !/*ty4a       12*/
      nl=lev-1                                                                  !/*ty4a       13*/
      do 538 il=1,itds(lr)                                                      !/*ty4a       14*/
      j=itds(lr+il)                                                             !/*ty4a       15*/
      do 536 in=1,itds(j)                                                       !/*ty4a       16*/
      j=j+1                                                                     !/*ty4a       17*/
      do 531 l=0,nl                                                             !/*ty4a       18*/
  531 if(abs(itds(j)).eq.nint(udt(7,l)))go to 532                               !/*ty4a       19*/
      if(il.gt.1.or.icl.ne.abs(itds(j)))go to 536                               !/*ty4a       20*/
      l=lev                                                                     !/*ty4a       21*/
  532 if(itds(j).gt.0)go to 538                                                 !/*ty4a       22*/
      do 534 i=1,3                                                              !/*ty4a       23*/
      ix=ijk(i)                                                                 !/*tyx        49*/
      if(l.lt.lev)ix=nint(udt(7+i,l))                                           !/*ty4a       25*/
  534 if(itds(j+i).ne.ix)go to 536                                              !/*ty4a       26*/
      go to 538                                                                 !/*ty4a       27*/
  536 if(itds(j).lt.0)j=j+3                                                     !/*ty4a       28*/
      go to 660                                                                 !/*ty4a       29*/
  538 nl=l-1                                                                    !/*ty4a       30*/
  539 mr=2                                                                      !/*ty4a       31*/
      mu=0                                                                      !/*ty        267*/
      td=tc                                                                     !/*ty        268*/
!                                                                               !/*ty        269*/
!        user bins loop.                                                        !/*ty        270*/
      if(iptal(li+3,1,ital).eq.0)go to 560                                      !/*ty        271*/
      if(lft(llft+4,ital)+lft(llft+7,ital)+lft(llft+9,ital).ne.0)               !/*ty        272*/
     & go to 560                                                                !/*ty        273*/
      nx=1                                                                      !/*ty        274*/
  540 mu=0                                                                      !/*ty        275*/
      td=tc                                                                     !/*ty        276*/
      ntx=nx                                                                    !/*ty        277*/
      call tallyx(td,mu)                                                        !/*ty        278*/
      if(mu.lt.0)go to 660                                                      !/*ty        279*/
      if(ibu.gt.0.and.ibu.lt.iptal(li+3,4,ital))go to 550                       !/*ty        280*/
      call errprn(1,ntbb(lntb+4,ital),2,zero+jptal(ljpt+1,ital),                !/*ty4b       12*/
     & zero+ibu,'tal','bin','tally not scored beyond last user bin.')           !/*ty4a       33*/
      go to 660                                                                 !/*ty        282*/
  550 nx=nx+1                                                                   !/*ty        283*/
      if(nx.le.iptal(li+1,5,ital)+1)go to 560                                   !/*ty        284*/
      call expirx(1,'tally','tallyx is in an endless loop.')                    !/*ty        285*/
      return                                                                    !/*ty        286*/
!                                                                               !/*ty        287*/
!        totals nest.                                                           !/*ty        288*/
  560 if(td.eq.0.)go to 650                                                     !/*ty        289*/
      j1=ktal+jptal(lj+5,ital)+iptal(li+2,5,ital)*(itds(lx+ir)-1)               !/*ty        290*/
      lc=iptal(li+6,4,ital)-ibc                                                 !/*ty        291*/
      lu=iptal(li+3,4,ital)-ibu                                                 !/*ty        292*/
      ls=iptal(li+4,4,ital)-ibs                                                 !/*ty        293*/
      me=iptal(li+7,4,ital)-ibe                                                 !/*ty        294*/
      mt=iptal(li+8,4,ital)-ibt                                                 !/*ty        295*/
      if(iptal(li+3,6,ital).ne.0)lu=1                                           !/*ty4c       40*/
      if(iptal(li+4,6,ital).ne.0)ls=1                                           !/*ty4c       41*/
      if(iptal(li+5,6,ital).ne.0)lm=1                                           !/*ty4c       42*/
      if(iptal(li+6,6,ital).ne.0)lc=1                                           !/*ty4c       43*/
      if(iptal(li+7,6,ital).ne.0)me=1                                           !/*ty4c       44*/
      if(iptal(li+8,6,ital).ne.0)mt=1                                           !/*ty4c       45*/
      ic=ibc                                                                    !/*ty        296*/
  570 j2=j1+(ic-1)*ik(7)                                                        !/*ty        297*/
      iu=ibu                                                                    !/*ty        298*/
  580 j3=j2+(iu-1)*ik(4)                                                        !/*ty        299*/
      is=ibs                                                                    !/*ty        300*/
  590 j4=j3+(is-1)*ik(5)                                                        !/*ty        301*/
      im=km                                                                     !/*ty        302*/
  600 j5=j4+(im-1)*ik(6)                                                        !/*ty        303*/
      id=1                                                                      !/*ty        304*/
  610 j6=j5+(id-1)*ik(3)                                                        !/*ty        305*/
      ie=ibe                                                                    !/*ty        306*/
  620 j7=j6+(ie-1)*ik(8)                                                        !/*ty        307*/
      it=ibt                                                                    !/*ty        308*/
  630 if(j7+it.eq.ktal+iwwg)call avrclc(td)                                     !/*ty4c       46*/
      if(tal(j7+it).ne.0.)go to 640                                             !/*ty        310*/
      jtls=jtls+1                                                               !/*ty        311*/
      if(jtls.le.ktls)tal(ktal+nmxf*mxf+jtls)=j7+it                             !/*ty4a       34*/
  640 tal(j7+it)=tal(j7+it)+td                                                  !/*ty        313*/
      do 645 i=1,npert                                                          !/*ty4b       13*/
  645 tal(j7+it+i*mxfp)=tal(j7+it+i*mxfp)+td*ptb(kptb+4,i)                      !/*ty4b       14*/
      it=it+mt                                                                  !/*ty        314*/
      if(it.le.jt)go to 630                                                     !/*ty        315*/
      ie=ie+me                                                                  !/*ty        316*/
      if(ie.le.je)go to 620                                                     !/*ty        317*/
      id=id+1                                                                   !/*ty        318*/
      if(id.le.jbd)go to 610                                                    !/*ty        319*/
      im=im+lm                                                                  !/*ty        320*/
      if(im.le.jm)go to 600                                                     !/*ty        321*/
      is=is+ls                                                                  !/*ty        322*/
      if(is.le.js)go to 590                                                     !/*ty        323*/
      iu=iu+lu                                                                  !/*ty        324*/
      if(iu.le.ju)go to 580                                                     !/*ty        325*/
      ic=ic+lc                                                                  !/*ty        326*/
      if(ic.le.jc)go to 570                                                     !/*ty        327*/
  650 if(mu.ne.0)go to 540                                                      !/*ty        328*/
  660 continue                                                                  !/*ty        329*/
  670 continue                                                                  !/*ty        330*/
!                                                                               !/*ty        331*/
!        see if there are any more time or segment bins to post.                !/*ty        332*/
      if(ll.eq.0)go to 690                                                      !/*ty        333*/
  680 ibt=ibt+1                                                                 !/*ty        334*/
      if(ibt.lt.iptal(li+8,4,ital))go to 510                                    !/*ty        335*/
  690 if(jptal(lj+2,ital).lt.4)go to 700                                        !/*ty        336*/
      if(dc.eq.dr)go to 700                                                     !/*ty        337*/
      da=dc                                                                     !/*ty        338*/
      if(dc.eq.db)go to 410                                                     !/*ty        339*/
      ibt=ibt+1                                                                 !/*ty        340*/
      if(ibt.lt.iptal(li+8,4,ital))go to 480                                    !/*ty        341*/
      call errprn(1,ntbb(lntb+3,ital),3,zero+jptal(ljpt+1,ital),tme,            !/*ty4b       15*/
     & 'tal','tme','tally not scored beyond last time bin.')                    !/*ty4a       36*/
  700 lx=lx+n1+2                                                                !/*ty        343*/
      tme=tv                                                                    !/*ty        344*/
      erg=es                                                                    !/*ty        345*/
      return                                                                    !/*ty        346*/
      end                                                                       !/*ty        360*/
