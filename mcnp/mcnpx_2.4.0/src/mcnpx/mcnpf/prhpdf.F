!#ident prhpdf.F from deck pg4a                                                 !/*pg4a        1*/
      subroutine prhpdf(bg,dn,ni,nn)                                            !/*pg4a        2*/
!        print the empirical history score probability density function.        !/*pg4a        3*/
!        bg=tally per history > 0 (< 0 neg dbcn(16)) below score grid.          !/*pg4a        4*/
!        dn=tally per history > 0 (< 0 neg dbcn(16)) for cum score norm.        !/*pg4a        5*/
!        ni=number of positve (neg if dbcn(16)<0) tfc history scores.           !/*pg4a        6*/
!        nn=number of entries in the tfc nps array npc.                         !/*pg4a        7*/
!        no print is done for 0 scores in hsb range or just one hsb bin.        !/*pg4a        8*/
      use global_data                                                           !/*pgx         1*/
!                                                                               !/*pgx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*pgx         3*/
      implicit integer(kindi) (i-n)                                             !/*pgx         4*/
!                                                                               !/*pgx         5*/
      dimension np(59),pd(59),pl(59),sb(60),sp(59)                              !/*pg4a       10*/
      character ch,da*100,dl(100),h(100),ha*8,hb*9,he*7,hf(2)*8,l1(100),        !/*pg4a       11*/
     & l2(100)                                                                  !/*pg4a       12*/
      data da/'--------10--------20--------30--------40--------50-------        !/*pg4a       13*/
     &-60--------70--------80--------90-------100'/,dl/100*'d'/,                !/*pg4a       14*/
     & h/9*' ','|',9*' ','|',9*' ','|',9*' ','|',9*' ','|',9*' ','|',           !/*pg4a       15*/
     & 9*' ','|',9*' ','|',9*' ','|',9*' ','|'/,l1/100*' '/,l2/100*' '/         !/*pg4a       16*/
!                                                                               !/*pg4a       17*/
!        set the upper(nu) and lower(nl) grid bin limits for score prd.         !/*pg4a       18*/
      it=ital+iper*ntal                                                         !/*pg4b        1*/
      nu=nhsd(lnhs+nsp+4,it)-1                                                  !/*pg4b        2*/
      nl=nhsd(lnhs+nsp+3,it)-1                                                  !/*pg4b        3*/
      if(nu-nl.gt.0)go to 20                                                    !/*pg4a       21*/
      write(iuo,10)hsb(nl),hsb(nl+1)                                            !/*pg4a       22*/
   10 format(46h print table 161 cannot be done for this tally,                 !/*pg4a       23*/
     & 33h because all tallies were between,1pe12.5,4h and,e12.5,               !/*pg4a       24*/
     & 24h which is one tally bin.)                                             !/*pg4a       25*/
      return                                                                    !/*pg4a       26*/
!                                                                               !/*pg4a       27*/
!        set flag no for tallies above hsb(nsp-1) and/or below hsb(1).          !/*pg4a       28*/
   20 no=0                                                                      !/*pg4a       29*/
      if(nhsd(lnhs+1,it).gt.0)no=1                                              !/*pg4b        4*/
      if(nhsd(lnhs+nsp,it).gt.0)no=no+2                                         !/*pg4b        5*/
      kp=59                                                                     !/*pg4a       32*/
      if(no.gt.0)kp=58                                                          !/*pg4a       33*/
!                                                                               !/*pg4a       34*/
!        calculate information arrays for the distribution tables.              !/*pg4a       35*/
!        nc is the number of bins to collapse to keep table on one page.        !/*pg4a       36*/
      nc=(nu-nl)/kp+1                                                           !/*pg4a       37*/
      pg=-huge                                                                  !/*pg4a       38*/
      ps=huge                                                                   !/*pg4a       39*/
      sb(1)=hsb(nl)                                                             !/*pg4a       40*/
!                                                                               !/*pg4a       41*/
!        loop over all of the tally distribution information.                   !/*pg4a       42*/
      k=0                                                                       !/*pg4a       43*/
      do 50 i=nl,nu,nc                                                          !/*pg4a       44*/
      k=k+1                                                                     !/*pg4a       45*/
      np(k)=0                                                                   !/*pg4a       46*/
      pd(k)=0.                                                                  !/*pg4a       47*/
      pl(k)=0.                                                                  !/*pg4a       48*/
      sp(k)=0.                                                                  !/*pg4a       49*/
!                                                                               !/*pg4a       50*/
!        collapse statistical tally information if necessary.                   !/*pg4a       51*/
      do 30 j=1,nc                                                              !/*pg4a       52*/
      if(i+j-1.gt.nu)go to 40                                                   !/*pg4a       53*/
      np(k)=np(k)+nhsd(lnhs+i+j,it)                                             !/*pg4b        6*/
   30 sp(k)=sp(k)+shsd(lshs+i+j,it)                                             !/*pg4b        7*/
   40 if(np(k).eq.0)go to 50                                                    !/*pg4a       56*/
!                                                                               !/*pg4a       57*/
!        calculate number density in tally bin pd and take the log10.           !/*pg4a       58*/
      pd(k)=np(k)*fpi/(hsb(i+j-1)-hsb(i))                                       !/*pg4a       59*/
      if(pd(k).gt.0.)pl(k)=log10(pd(k))                                         !/*pg4a       60*/
      ps=min(ps,pl(k))                                                          !/*pg4a       61*/
      pg=max(pg,pl(k))                                                          !/*pg4a       62*/
   50 sb(k+1)=hsb(i+j-1)                                                        !/*pg4a       63*/
!                                                                               !/*pg4a       64*/
!        set up constants to give from 1 to 100 characters on a line.           !/*pg4a       65*/
   60 d=100./(1.01*(pg-ps))                                                     !/*pg4a       66*/
      pt=1.01*ps-.01*pg                                                         !/*pg4a       67*/
!                                                                               !/*pg4a       68*/
!        create a character line with a 'd' approximately every decade.         !/*pg4a       69*/
      j=1                                                                       !/*pg4a       70*/
      ld=nint(d)                                                                !/*pg4a       71*/
      ls=ld                                                                     !/*pg4a       72*/
      do 70 i=2,100                                                             !/*pg4a       73*/
      dl(i)='-'                                                                 !/*pg4a       74*/
      l1(i)=' '                                                                 !/*pg4a       75*/
      if(mod(i,ld).ne.0)go to 70                                                !/*pg4a       76*/
      dl(i)='d'                                                                 !/*pg4a       77*/
      if(ls.gt.6.or.mod(j,3).eq.0)l1(i)='|'                                     !/*pg4a       78*/
      j=j+1                                                                     !/*pg4a       79*/
      ld=nint(j*d)                                                              !/*pg4a       80*/
   70 continue                                                                  !/*pg4a       81*/
!                                                                               !/*pg4a       82*/
!        set some character and other variables for later use.                  !/*pg4a       83*/
      t5=min(ten,tfc(ltfc+5,nn,it))                                             !/*pgx         6*/
      if(t5.le.1..and.t5.ne.0.)t5=10.                                           !/*pg4a       85*/
      t6=tfc(ltfc+6,nn,it)                                                      !/*pg4b        9*/
      tb=shsd(lshs+nspt,it)                                                     !/*pg4b       10*/
      if(t5.eq.0..or.t5.eq.10.)tb=hsb(nsp-1)                                    !/*pg4a       88*/
      hb=',s=slope)'                                                            !/*pg4a       89*/
      if(tb.eq.hsb(nsp-1))hb=')        '                                        !/*pg4a       90*/
      he='       '                                                              !/*pg4a       91*/
      if(hsb(nsp).lt.0.)he='[f(-x)]'                                            !/*pg4a       92*/
      j1=jptal(ljpt+1,ital)                                                     !/*pg4a       93*/
!                                                                               !/*pg4a       94*/
!        print the history tally probability density function plot.             !/*pg4a       95*/
      sm=hsb(nsp)*shsd(lshs+nsp+2,it)/nhsd(lnhs+nsp+2,it)                       !/*pg4b       11*/
      write(iuo,80)j1,he,sm,nps,t5,(dl(j),j=1,100)                              !/*pg4a       97*/
   80 format(33h1unnormed tally density for tally,i4,1x,a7,4x,7hnonzero,        !/*pg4a       98*/
     & 16h tally mean(m) =,1pe10.3,3x,5hnps =,i10,4x,15hprint table 161         !/*pg4a       99*/
     & //9h abscissa,13x,9h ordinate,2x,18h log plot of tally,                  !/*pg4a      100*/
     & 60h probability density function in tally fluctuation chart bin,         !/*pg4a      101*/
     & 16h(d=decade,slope=,0pf4.1,1h)/                                          !/*pg4a      102*/
     & 32h  tally  number num den log den:,100a1)                               !/*pg4a      103*/
!                                                                               !/*pg4a      104*/
!        loop over k table lines and check for mean(m) line.                    !/*pg4a      105*/
      ny=0                                                                      !/*pg4a      106*/
      do 160 i=1,k                                                              !/*pg4a      107*/
      n=0                                                                       !/*pg4a      108*/
      if(np(i).gt.0.)n=nint((pl(i)-pt)*d)                                       !/*pg4a      109*/
      ch='*'                                                                    !/*pg4a      110*/
      if(sm.gt.sb(i).and.sm.le.sb(i+1))ch='m'                                   !/*pg4a      111*/
!                                                                               !/*pg4a      112*/
!        delete the e from two floating point numbers for tight print.          !/*pg4a      113*/
      write(hb,'(1pe9.2)')sb(i+1)                                               !/*pg4a      114*/
      hf(1)=hb(1:5)//hb(7:9)                                                    !/*pg4a      115*/
      write(hb,'(1pe9.2)')pd(i)                                                 !/*pg4a      116*/
      hf(2)=hb(1:5)//hb(7:9)                                                    !/*pg4a      117*/
!                                                                               !/*pg4a      118*/
!        create a line for the print plot for the ith line.                     !/*pg4a      119*/
      do 90 j=1,100                                                             !/*pg4a      120*/
   90 l2(j)=l1(j)                                                               !/*pg4a      121*/
      do 100 j=1,n                                                              !/*pg4a      122*/
  100 if(l2(j).ne.'|')l2(j)=ch                                                  !/*pg4a      123*/
!                                                                               !/*pg4a      124*/
!        set location of s for printed pareto tail plot at average sb.          !/*pg4a      125*/
!        log point for a pareto is t5*log10(1.+t6*(avg sb)/(t5-1.))             !/*pg4a      126*/
!        location nz=ny-d*(log10(sb(i+1)>tb)-log10(first sb(i+1)>tb)).          !/*pg4a      127*/
      if(tb.ge.sb(i+1))go to 120                                                !/*pg4a      128*/
      if(ny.ne.0)go to 110                                                      !/*pg4a      129*/
      ny=min(n+10,100)                                                          !/*pg4a      130*/
      if(l2(ny).eq.'|')ny=ny-1                                                  !/*pg4a      131*/
      l2(ny)='s'                                                                !/*pg4a      132*/
      ty=ny+d*t5*log10(t6*(1.+t6*(0.7*sb(i)+0.3*sb(i+1))/(t5-1.)))              !/*pg4a      133*/
      go to 120                                                                 !/*pg4a      134*/
  110 nz=nint(ty-d*t5*log10(t6*(1.+t6*(0.7*sb(i)+0.3*sb(i+1))/(t5-1.))))        !/*pg4a      135*/
      if(nz.gt.1.and.nz.le.100)l2(nz)='s'                                       !/*pg4a      136*/
!                                                                               !/*pg4a      137*/
!        write a line of the printed plot of the history tally pdf.             !/*pg4a      138*/
  120 if(np(i).gt.999999)go to 140                                              !/*pg4a      139*/
      write(iuo,130)hf(1),np(i),hf(2),pl(i),(l2(j),j=1,100)                     !/*pg4a      140*/
  130 format(a8,i7,a8,f8.3,1x,100a1)                                            !/*pg4a      141*/
      go to 160                                                                 !/*pg4a      142*/
  140 write(ha,'(1pe8.1)')float(np(i))                                          !/*pg4a      143*/
      he=ha(1:4)//ha(6:8)                                                       !/*pg4a      144*/
      write(iuo,150)hf(1),he,hf(2),pl(i),(l2(j),j=1,100)                        !/*pg4a      145*/
  150 format(a8,a7,a8,f8.3,1x,100a1)                                            !/*pg4a      146*/
  160 continue                                                                  !/*pg4a      147*/
!                                                                               !/*pg4a      148*/
!        print the total line for the printed history tally pdf plot.           !/*pg4a      149*/
      ns=nhsd(lnhs+nsp+2,it)-nhsd(lnhs+1,it)-nhsd(lnhs+nsp,it)                  !/*pg4b       12*/
      write(hb,'(1pe9.2)')float(ns)                                             !/*pg4a      151*/
      hf(1)=hb(1:5)//hb(7:9)                                                    !/*pg4a      152*/
      write(hb,'(1pe9.2)')ns*fpi                                                !/*pg4a      153*/
      hf(2)=hb(1:5)//hb(7:9)                                                    !/*pg4a      154*/
      if(ns.le.9999999)write(iuo,170)ns,hf(2),(dl(j),j=1,100)                   !/*pg4a      155*/
  170 format(2x,5htotal,i8,a8,9x,100a1)                                         !/*pg4a      156*/
      if(ns.gt.9999999)write(iuo,180)hf(1),hf(2),(dl(j),j=1,100)                !/*pg4a      157*/
  180 format(2x,5htotal,2a8,9x,100a1)                                           !/*pg4a      158*/
!                                                                               !/*pg4a      159*/
!        write any history tallies that were outside the values in hsb.         !/*pg4a      160*/
      kp=26                                                                     !/*pg4a      161*/
      if(no.eq.0)go to 220                                                      !/*pg4a      162*/
      kp=25                                                                     !/*pg4a      163*/
      if(no.eq.1)write(iuo,190)nhsd(lnhs+1,it)                                  !/*pg4b       13*/
  190 format(11h a total of,i10,                                                !/*pg4a      165*/
     & 50h tallies were below the score grid bin boundaries.)                   !/*pg4a      166*/
      if(no.eq.2)write(iuo,200)nhsd(lnhs+nsp,it)                                !/*pg4b       14*/
  200 format(11h a total of,i10,                                                !/*pg4a      168*/
     & 50h tallies were above the score grid bin boundaries.)                   !/*pg4a      169*/
      if(no.eq.3)write(iuo,210)nhsd(lnhs+1,it),nhsd(lnhs+nsp,it)                !/*pg4b       15*/
  210 format(11h a total of,i10,23h tallies were below and,i10,                 !/*pg4a      171*/
     & 50h tallies were above the score grid bin boundaries.)                   !/*pg4a      172*/
  220 if(ink(162).eq.0.and.k.gt.kp)return                                       !/*pg4a      173*/
!                                                                               !/*pg4a      174*/
!        calculate and print the cumulative tally number table.                 !/*pg4a      175*/
      ch='1'                                                                    !/*pg4a      176*/
      if(k.gt.kp)go to 240                                                      !/*pg4a      177*/
      ch=' '                                                                    !/*pg4a      178*/
      write(iuo,230)                                                            !/*pg4a      179*/
  230 format(/)                                                                 !/*pg4a      180*/
  240 write(iuo,250)ch,j1,he,sm,nps                                             !/*pg4a      181*/
  250 format(a1,33hcumulative tally number for tally,i4,1x,a7,3x,               !/*pg4a      182*/
     & 23hnonzero tally mean(m) =,1pe10.3,3x,5hnps =,i10,4x,                    !/*pg4a      183*/
     & 15hprint table 162)                                                      !/*pg4a      184*/
      write(iuo,260)da                                                          !/*pg4a      185*/
  260 format(/11h   abscissa,4x,15h cum   ordinate,3x,12h plot of the,          !/*pg4a      186*/
     & 60h cumulative number of tallies in the tally fluctuation chart,         !/*pg4a      187*/
     & 26h bin from 0 to 100 percent/31h    tally      number  cum pct:,        !/*pg4a      188*/
     & a100)                                                                    !/*pg4a      189*/
!                                                                               !/*pg4a      190*/
!        loop over k table lines and check for mean(m) line.                    !/*pg4a      191*/
      n=nhsd(lnhs+1,it)-nhsd(lnhs+nsp+1,it)                                     !/*pg4b       16*/
      do 320 i=1,k                                                              !/*pg4a      193*/
      n=n+np(i)                                                                 !/*pg4a      194*/
      ch='*'                                                                    !/*pg4a      195*/
      if(sm.gt.sb(i).and.sm.le.sb(i+1))ch='m'                                   !/*pg4a      196*/
      do 270 j=1,100                                                            !/*pg4a      197*/
  270 l2(j)=h(j)                                                                !/*pg4a      198*/
      l=nint(100.*n/ni)                                                         !/*pg4a      199*/
      do 280 j=1,l                                                              !/*pg4a      200*/
  280 if(l2(j).ne.'|')l2(j)=ch                                                  !/*pg4a      201*/
      if(n.gt.9999999)go to 300                                                 !/*pg4a      202*/
      write(iuo,290)sb(i+1),n,100.*n/ni,(l2(j),j=1,100)                         !/*pg4a      203*/
  290 format(1pe12.5,i9,0pf9.3,1h|,100a1)                                       !/*pg4a      204*/
      go to 320                                                                 !/*pg4a      205*/
  300 write(hb,'(1pe9.2)')float(n)                                              !/*pg4a      206*/
      hf(1)=hb(1:5)//hb(7:9)                                                    !/*pg4a      207*/
      write(iuo,310)sb(i+1),hf(1),100.*n/ni,(l2(j),j=1,100)                     !/*pg4a      208*/
  310 format(1pe12.5,1x,a8,0pf9.3,1h|,100a1)                                    !/*pg4a      209*/
  320 continue                                                                  !/*pg4a      210*/
      write(iuo,330)ns,100.*n/nhsd(lnhs+nsp+2,it),da                            !/*pg4b       17*/
  330 format(4x,5htotal,i12,f9.3,1h:,a100)                                      !/*pg4a      212*/
!                                                                               !/*pg4a      213*/
!        write any history tallies that were outside the values in hsb.         !/*pg4a      214*/
      kp=15                                                                     !/*pg4a      215*/
      if(no.eq.0)go to 340                                                      !/*pg4a      216*/
      kp=14                                                                     !/*pg4a      217*/
      if(no.eq.1)write(iuo,190)nhsd(lnhs+1,it)                                  !/*pg4b       18*/
      if(no.eq.2)write(iuo,200)nhsd(lnhs+nsp,it)                                !/*pg4b       19*/
      if(no.eq.3)write(iuo,210)nhsd(lnhs+1,it),nhsd(lnhs+nsp,it)                !/*pg4b       20*/
  340 if(ink(162).eq.0.and.k.gt.kp)return                                       !/*pg4a      221*/
!                                                                               !/*pg4a      222*/
!        calculate and print the cumulative history tally table.                !/*pg4a      223*/
      ch='1'                                                                    !/*pg4a      224*/
      if(k.gt.kp)go to 350                                                      !/*pg4a      225*/
      ch=' '                                                                    !/*pg4a      226*/
      write(iuo,230)                                                            !/*pg4a      227*/
  350 write(iuo,360)ch,j1,he,sm,nps                                             !/*pg4a      228*/
  360 format(a1,35hcumulative unnormed tally for tally,i4,1x,a7,1x,             !/*pg4a      229*/
     & 23hnonzero tally mean(m) =,1pe10.3,3x,5hnps =,i10,4x,                    !/*pg4a      230*/
     & 15hprint table 162)                                                      !/*pg4a      231*/
      write(iuo,370)da                                                          !/*pg4a      232*/
  370 format(/10h  abscissa,4x,4h cum,3x,9h ordinate,15x,8h plot of,            !/*pg4a      233*/
     & 61h the cumulative tally in the tally fluctuation chart bin from,        !/*pg4a      234*/
     & 17h 0 to 100 percent/31h   tally    tally/nps  cum pct:,a100)            !/*pg4a      235*/
!                                                                               !/*pg4a      236*/
!        loop over k table lines and check for mean(m) line.                    !/*pg4a      237*/
      do 400 i=1,k                                                              !/*pg4a      238*/
      bg=bg+sp(i)*fpi                                                           !/*pg4a      239*/
      ch='*'                                                                    !/*pg4a      240*/
      if(sm.gt.sb(i).and.sm.le.sb(i+1))ch='m'                                   !/*pg4a      241*/
      do 380 j=1,100                                                            !/*pg4a      242*/
  380 l2(j)=h(j)                                                                !/*pg4a      243*/
      l=nint(100.*bg/dn)                                                        !/*pg4a      244*/
      do 390 j=1,l                                                              !/*pg4a      245*/
  390 if(l2(j).ne.'|')l2(j)=ch                                                  !/*pg4a      246*/
  400 write(iuo,410)sb(i+1),bg,bg/dn,(l2(j),j=1,100)                            !/*pg4a      247*/
  410 format(1pe10.3,e11.3,1x,2pf8.3,1h|,100a1)                                 !/*pg4a      248*/
      write(iuo,420)(shsd(lshs+nsp+2,it)-shsd(lshs+1,it)-                       !/*pg4b       21*/
     & shsd(lshs+nsp,it))*fpi,bg/dn,da                                          !/*pg4b       22*/
  420 format(3x,5htotal,1pe13.5,1x,2pf8.3,1h:,a100)                             !/*pg4a      251*/
!                                                                               !/*pg4a      252*/
!        write any history tallies that were outside the values in hsb.         !/*pg4a      253*/
      if(no.eq.0)return                                                         !/*pg4a      254*/
      if(no.eq.1)write(iuo,430)hsb(nsp)*shsd(lshs+1,it)*fpi                     !/*pg4b       23*/
  430 format(21h a total tally/nps of,1pe12.5,                                  !/*pg4a      256*/
     & 41h was below the score grid bin boundaries.)                            !/*pg4a      257*/
      if(no.eq.2)write(iuo,440)hsb(nsp)*shsd(lshs+nsp,it)*fpi                   !/*pg4b       24*/
  440 format(21h a total tally/nps of,1pe12.5,                                  !/*pg4a      259*/
     & 41h was above the score grid bin boundaries.)                            !/*pg4a      260*/
      if(no.eq.3)write(iuo,450)hsb(nsp)*shsd(lshs+1,it)*fpi,                    !/*pg4b       25*/
     & hsb(nsp)*shsd(lshs+nsp,it)*fpi                                           !/*pg4b       26*/
  450 format(21h total tallies/nps of,1pe12.5,15h were below and,               !/*pg4a      263*/
     & e12.5,42h were above the score grid bin boundaries.)                     !/*pg4a      264*/
      return                                                                    !/*pg4a      265*/
      end                                                                       !/*pg4a      266*/
