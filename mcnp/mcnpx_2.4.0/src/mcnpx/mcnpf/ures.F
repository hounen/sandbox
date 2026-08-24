!#ident ures.F from deck ur4c                                                   !/*ur4c        1*/
      subroutine ures(ic)                                                       !/*ur4c        2*/
!        sample cross sections from probability tables for unresolved           !/*ur4c        3*/
!        resonance range and put in rtc( ,iex) array. ic = energy index.        !/*ur4c        4*/
!        l=jxs(ljxs+23,iex) points to location for unres prob tables;           !/*ur4c        5*/
!            xss(l)  = number of incident energies                              !/*ur4c        6*/
!            xss(l+1)= length of a table (usually 20)                           !/*ur4c        7*/
!            xss(l+2)= incident energy interpolation parameter (2 or 5)         !/*ur4c        8*/
!            xss(l+3)= flag for inelastic (-1 none, 0 by balance,>0=mt)         !/*ur4c        9*/
!            xss(l+4)= flag for "other absorption" (-1 none, etc.)              !/*ur4c       10*/
!            xss(l+5)= flag for factors, if 0, regular cross sections           !/*ur4c       11*/
!            followed by incident energies                                      !/*ur4c       12*/
!            followed by tables for each incident energy (6 columns)            !/*ur4c       13*/
!               cumulative probabilities                                        !/*ur4c       14*/
!               total cross sections                                            !/*ur4c       15*/
!               elastic cross sections                                          !/*ur4c       16*/
!               fission cross sections                                          !/*ur4c       17*/
!               capture cross sections (102)                                    !/*ur4c       18*/
!               neutron heating numbers                                         !/*ur4c       19*/
!        repeat for each incident energy.                                       !/*ur4c       20*/
!                                                                               !/*ur4c       21*/
!        rtc( 1,iex) = interpolation fraction                                   !/*ur4c       22*/
!        rtc( 2,iex) = interpolation fraction after free gas thermal            !/*ur4c       23*/
!        rtc( 3,iex) = absorption cross section                                 !/*ur4c       24*/
!                    = rtc(14,iex) + any "other absorption"                     !/*ur4c       25*/
!        rtc( 4,iex) = total cross section                                      !/*ur4c       26*/
!        rtc( 5,iex) = rtc( 4,iex) = total xsec after free gas thermal          !/*ur4c       27*/
!        rtc( 6,iex) = neutron energy                                           !/*ur4c       28*/
!        rtc( 7,iex) = thermal temperature (unused for unresolved)              !/*ur4c       29*/
!        rtc( 8,iex) = zero or fission                                          !/*ur4c       30*/
!        rtc(11,iex) = elastic xsec (-1 if not in unresolved range)             !/*ur4c       31*/
!        rtc(12,iex) = fission xsec                                             !/*ur4c       32*/
!        rtc(13,iex) = neutron heating number                                   !/*ur4c       33*/
!        rtc(14,iex) = (n,gamma) capture xsec                                   !/*ur4c       34*/
!        rtc(15,iex) = random number used to sample xsecs                       !/*ur4c       35*/
      use global_data                                                           !/*urx         1*/
!                                                                               !/*urx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*urx         3*/
      implicit integer(kindi) (i-n)                                             !/*urx         4*/
!                                                                               !/*urx         5*/
      n=nxs(lnxs+3,iex)                                                         !/*ur4c       37*/
      l=jxs(ljxs+23,iex)                                                        !/*ur4c       38*/
      n1=nint(xss(l))                                                           !/*ur4c       39*/
      ke=l+6                                                                    !/*ur4c       40*/
      n2=nint(xss(l+1))                                                         !/*ur4c       41*/
      je=ke+1                                                                   !/*ur4c       42*/
      jf=0                                                                      !/*ur4c       43*/
!                                                                               !/*ur4c       44*/
!        find energy table.                                                     !/*ur4c       45*/
   10 if(erg.lt.xss(je))go to 20                                                !/*ur4c       46*/
      je=je+1                                                                   !/*ur4c       47*/
      go to 10                                                                  !/*ur4c       48*/
!                                                                               !/*ur4c       49*/
!        get fractional distance between incident energy prob. tables.          !/*ur4c       50*/
   20 r=(erg-xss(je-1))/(xss(je)-xss(je-1))                                     !/*ur4c       51*/
      jn=je-ke                                                                  !/*ur4c       52*/
      ja=ke+n1+(jn-1)*n2*6                                                      !/*ur4c       53*/
      j=ja                                                                      !/*ur4c       54*/
!                                                                               !/*ur4c       55*/
!        if correlated by temperature, find random number already used.         !/*ur4c       56*/
      if(iunr.ge.0)go to 40                                                     !/*ur4c       57*/
      do 30 ie=1,mxe                                                            !/*ur4c       58*/
      if(ie.eq.iex)go to 30                                                     !/*ur4c       59*/
      if(nty(lnty+ie).ne.1)go to 30                                             !/*ur4c       60*/
      if(nxs(lnxs+2,ie).ne.nxs(lnxs+2,iex))go to 30                             !/*ur4c       61*/
      if(rtc(krtc+6,ie).ne.erg)go to 30                                         !/*ur4c       62*/
      lk=jxs(ljxs+23,ie)                                                        !/*ur4c       63*/
      if(lk.eq.0)go to 30                                                       !/*ur4c       64*/
      if(erg.le.xss(lk+6))go to 30                                              !/*ur4c       65*/
      if(erg.ge.xss(lk+5+nint(xss(lk))))go to 30                                !/*ur4c       66*/
      if(rtc(krtc+11,ie).lt.0.)go to 30                                         !/*ur4c       67*/
      rn=rtc(krtc+15,ie)                                                        !/*ur4c       68*/
      go to 50                                                                  !/*ur4c       69*/
   30 continue                                                                  !/*ur4c       70*/
   40 rn=rang()                                                                 !/*ur4c       71*/
!                                                                               !/*ur4c       72*/
!        sample from the probability tables.                                    !/*ur4c       73*/
   50 rtc(krtc+15,iex)=rn                                                       !/*ur4c       74*/
   60 if(xss(j).ge.rn)go to 70                                                  !/*ur4c       75*/
      j=j+1                                                                     !/*ur4c       76*/
      go to 60                                                                  !/*ur4c       77*/
   70 if(j-ja+1.gt.n2)call expirx(0,'ures','prob table not normed.')            !/*urxk        1*/
      if(jf.ne.0)go to 80                                                       !/*ur4c       79*/
!        set locator in probability table for lower incident energy.            !/*ur4c       80*/
      j1=j                                                                      !/*ur4c       81*/
      ja=ja+n2*6                                                                !/*ur4c       82*/
      j=ja                                                                      !/*ur4c       83*/
      jf=1                                                                      !/*ur4c       84*/
      go to 60                                                                  !/*ur4c       85*/
!                                                                               !/*ur4c       86*/
!        set locator in probability table for upper incident energy.            !/*ur4c       87*/
   80 j2=j                                                                      !/*ur4c       88*/
      ki=nint(xss(l+2))                                                         !/*ur4c       89*/
!                                                                               !/*ur4c       90*/
!        obtain capture, elastic, fission, and heating from prob tables.        !/*ur4c       91*/
!        use lin-lin interpolation.                                             !/*ur4c       92*/
      if(ki.eq.5.and.r.gt.0..and.r.lt.1.)go to 90                               !/*ur4c       93*/
      rtc(krtc+3,iex)=xss(j1+n2*4)+r*(xss(j2+n2*4)-xss(j1+n2*4))                !/*ur4c       94*/
      rtc(krtc+11,iex)=xss(j1+n2*2)+r*(xss(j2+n2*2)-xss(j1+n2*2))               !/*ur4c       95*/
      rtc(krtc+12,iex)=xss(j1+n2*3)+r*(xss(j2+n2*3)-xss(j1+n2*3))               !/*ur4c       96*/
      rtc(krtc+13,iex)=xss(j1+n2*5)+r*(xss(j2+n2*5)-xss(j1+n2*5))               !/*ur4c       97*/
      go to 100                                                                 !/*ur4c       98*/
!                                                                               !/*ur4c       99*/
!        use log-log interpolation.                                             !/*ur4c      100*/
   90 g=log(erg/xss(je-1))/log(xss(je)/xss(je-1))                               !/*ur4c      101*/
      i=n2*4                                                                    !/*ur4c      102*/
      rtc(krtc+3,iex)=0.                                                        !/*ur4c      103*/
      if(xss(j1+i).ne.0.)                                                       !/*ur4c      104*/
     & rtc(krtc+3,iex)=exp(log(xss(j1+i))+g*log(xss(j2+i)/xss(j1+i)))           !/*ur4c      105*/
      i=n2*2                                                                    !/*ur4c      106*/
      rtc(krtc+11,iex)=exp(log(xss(j1+i))+g*log(xss(j2+i)/xss(j1+i)))           !/*ur4c      107*/
      i=n2*3                                                                    !/*ur4c      108*/
      rtc(krtc+12,iex)=0.                                                       !/*ur4c      109*/
      if(xss(j1+i).ne.0.)                                                       !/*ur4c      110*/
     & rtc(krtc+12,iex)=exp(log(xss(j1+i))+g*log(xss(j2+i)/xss(j1+i)))          !/*ur4c      111*/
      i=n2*5                                                                    !/*ur4c      112*/
      rtc(krtc+13,iex)=exp(log(xss(j1+i))+g*log(xss(j2+i)/xss(j1+i)))           !/*ur4c      113*/
!                                                                               !/*ur4c      114*/
!        get cross sections by factors (xss(l+5).ne.0) or get                   !/*ur4c      115*/
!        inelastic cross section by balance (xss(l+3).eq.0).                    !/*ur4c      116*/
!        ce and cf are the elastic and fission factors.                         !/*ur4c      117*/
  100 if(xss(l+5).eq.0..and.xss(l+3).ne.0.)go to 110                            !/*ur4c      118*/
      ce=xss(ic+3*n)+rtc(krtc+1,iex)*(xss(ic+3*n+1)-xss(ic+3*n))                !/*ur4c      119*/
      cf=0.                                                                     !/*ur4c      120*/
      i=jxs(ljxs+21,iex)                                                        !/*ur4c      121*/
      if(i.eq.0)go to 110                                                       !/*ur4c      122*/
      ii=ktc(kktc+1,iex)-nint(xss(i))+1                                         !/*ur4c      123*/
      if(ii.lt.1)go to 110                                                      !/*ur4c      124*/
      ii=ii+i+1                                                                 !/*ur4c      125*/
      cf=xss(ii)+rtc(krtc+1,iex)*(xss(ii+1)-xss(ii))                            !/*ur4c      126*/
!                                                                               !/*ur4c      127*/
!        get cc, other absorption factor.                                       !/*ur4c      128*/
  110 if(xss(l+5).eq.0..and.xss(l+4).ne.0.)go to 150                            !/*ur4c      129*/
      cc=0.                                                                     !/*ur4c      130*/
      i=jxs(ljxs+3,iex)-1                                                       !/*ur4c      131*/
      do 120 ix=1,nxs(lnxs+4,iex)                                               !/*ur4c      132*/
  120 if(nint(xss(i+ix)).eq.102)go to 130                                       !/*ur4c      133*/
      go to 140                                                                 !/*ur4c      134*/
  130 iq=jxs(ljxs+7,iex)+nint(xss(jxs(ljxs+6,iex)+ix-1))                        !/*ur4c      135*/
      jq=ktc(kktc+1,iex)+1-nint(xss(iq-1))                                      !/*ur4c      136*/
      if(jq.lt.1.or.jq.gt.nint(xss(iq)))go to 140                               !/*ur4c      137*/
      if(jq.eq.nint(xss(iq)).and.rtc(krtc+1,iex).ne.0.)go to 140                !/*ur4c      138*/
      cc=xss(iq+jq)                                                             !/*ur4c      139*/
      if(rtc(krtc+1,iex).ne.0.)cc=cc+                                           !/*ur4c      140*/
     & rtc(krtc+1,iex)*(xss(iq+jq+1)-xss(iq+jq))                                !/*ur4c      141*/
!                                                                               !/*ur4c      142*/
!        probability tables are factors (xss(l+5).ne.0), so multiply            !/*ur4c      143*/
!        smooth xsecs by factors sampled from probability tables.               !/*ur4c      144*/
!        ch is heating number factor.                                           !/*ur4c      145*/
  140 if(xss(l+5).eq.0.)go to 150                                               !/*ur4c      146*/
      ch=xss(ic+4*n)+rtc(krtc+1,iex)*(xss(ic+4*n+1)-xss(ic+4*n))                !/*ur4c      147*/
      rtc(krtc+11,iex)=rtc(krtc+11,iex)*ce                                      !/*ur4c      148*/
      rtc(krtc+12,iex)=rtc(krtc+12,iex)*cf                                      !/*ur4c      149*/
      rtc(krtc+3,iex)=rtc(krtc+3,iex)*cc                                        !/*ur4c      150*/
      rtc(krtc+13,iex)=rtc(krtc+13,iex)*ch                                      !/*ur4c      151*/
!                                                                               !/*ur4c      152*/
!        obtain prob table total as sum of partials to avoid roundoff.          !/*ur4c      153*/
!        rtc(14,iex) is 102 capture and rtc(3,iex) is absorption.               !/*ur4c      154*/
  150 rtc(krtc+4,iex)=rtc(krtc+11,iex)+rtc(krtc+12,iex)+rtc(krtc+3,iex)         !/*ur4c      155*/
      if(jxs(ljxs+2,iex).eq.0.and.rtc(krtc+12,iex).ne.0.)call expirx(0,         !/*urxk        2*/
     & 'ures','fission conflict with unresolved prob table.')                   !/*ur4c      157*/
      rtc(krtc+14,iex)=rtc(krtc+3,iex)                                          !/*ur4c      158*/
!                                                                               !/*ur4c      159*/
!        obtain inelastic, r1, and other absorption, r2, by balance             !/*ur4c      160*/
!        using smooth cross sections or from the mt numbers.                    !/*ur4c      161*/
      r1=0.                                                                     !/*ur4c      162*/
      r2=0.                                                                     !/*ur4c      163*/
!                                                                               !/*ur4c      164*/
!        get ch, absorption factor, if (xss(l+3).eq.0) inelastic by             !/*ur4c      165*/
!        balance or (xss(l+4).eq.0) other absorption flag set.                  !/*ur4c      166*/
      if(xss(l+3).eq.0..or.xss(l+4).eq.0.)ca=xss(ic+2*n)+                       !/*ur4c      167*/
     & rtc(krtc+1,iex)*(xss(ic+2*n+1)-xss(ic+2*n))                              !/*ur4c      168*/
      if(xss(l+3).gt.0.)go to 160                                               !/*ur4c      169*/
      if(xss(l+3).lt.0.)go to 190                                               !/*ur4c      170*/
!                                                                               !/*ur4c      171*/
!        xss(l+3)=0:  get total inelastic by balance.  get smooth total,        !/*ur4c      172*/
!        ct, then subtract smooth elastic, fission, and absorption.             !/*ur4c      173*/
      ct=xss(ic+n)+rtc(krtc+1,iex)*(xss(ic+n+1)-xss(ic+n))                      !/*ur4c      174*/
      r1=ct-ce-cf-ca                                                            !/*ur4c      175*/
      go to 190                                                                 !/*ur4c      176*/
!                                                                               !/*ur4c      177*/
!        xss(l+3)>0.  get inelastic from mt.                                    !/*ur4c      178*/
!        jxs(ljxs+23,iex)+3 contains the mt of inelastic.                       !/*ur4c      179*/
  160 mt=nint(xss(l+3))                                                         !/*ur4c      180*/
      i=jxs(ljxs+3,iex)-1                                                       !/*ur4c      181*/
      do 170 ix=1,nxs(lnxs+4,iex)                                               !/*ur4c      182*/
  170 if(nint(xss(i+ix)).eq.mt)go to 180                                        !/*ur4c      183*/
      call expirx(0,'ures','total inelastic mt not in table.')                  !/*urxk        3*/
  180 is=jxs(ljxs+7,iex)+nint(xss(jxs(ljxs+6,iex)+ix-1))                        !/*ur4c      185*/
      ic=ktc(kktc+1,iex)+1-nint(xss(is-1))                                      !/*ur4c      186*/
      if(ic.lt.1.or.ic.gt.nint(xss(is)))go to 190                               !/*ur4c      187*/
      if(ic.eq.nint(xss(is)).and.rtc(krtc+1,iex).ne.0.)go to 190                !/*ur4c      188*/
      r1=xss(is+ic)                                                             !/*ur4c      189*/
      if(rtc(krtc+1,iex).ne.0.)r1=r1+                                           !/*ur4c      190*/
     & rtc(krtc+1,iex)*(xss(is+ic+1)-xss(is+ic))                                !/*ur4c      191*/
!                                                                               !/*ur4c      192*/
!        check flag for other absorption to include in abs and total.           !/*ur4c      193*/
!        obtain other absorption by smooth (abs-102).                           !/*ur4c      194*/
  190 if(xss(l+4).lt.0.)go to 230                                               !/*ur4c      195*/
      if(xss(l+4).gt.0.)go to 200                                               !/*ur4c      196*/
      r2=ca-cc                                                                  !/*ur4c      197*/
      go to 230                                                                 !/*ur4c      198*/
!                                                                               !/*ur4c      199*/
!        jxs(ljxs+23,iex)+4 contains the mt of other absorption.                !/*ur4c      200*/
  200 mt=nint(xss(l+4))                                                         !/*ur4c      201*/
      i=jxs(ljxs+3,iex)-1                                                       !/*ur4c      202*/
      do 210 ix=1,nxs(lnxs+4,iex)                                               !/*ur4c      203*/
  210 if(nint(xss(i+ix)).eq.mt)go to 220                                        !/*ur4c      204*/
      call expirx(0,'ures','other abs mt not in table.')                        !/*urxk        4*/
  220 is=jxs(ljxs+7,iex)+nint(xss(jxs(ljxs+6,iex)+ix-1))                        !/*ur4c      206*/
      ic=ktc(kktc+1,iex)+1-nint(xss(is-1))                                      !/*ur4c      207*/
      if(ic.lt.1.or.ic.gt.nint(xss(is)))go to 230                               !/*ur4c      208*/
      if(ic.eq.nint(xss(is)).and.rtc(krtc+1,iex).ne.0.)go to 230                !/*ur4c      209*/
      r2=xss(is+ic)                                                             !/*ur4c      210*/
      if(rtc(krtc+1,iex).ne.0.)r2=r2+                                           !/*ur4c      211*/
     & rtc(krtc+1,iex)*(xss(is+ic+1)-xss(is+ic))                                !/*ur4c      212*/
!                                                                               !/*ur4c      213*/
!        increment cross sections for inelastic and other absorption.           !/*ur4c      214*/
  230 rtc(krtc+3,iex)=rtc(krtc+3,iex)+r2                                        !/*ur4c      215*/
      rtc(krtc+4,iex)=rtc(krtc+4,iex)+r1+r2                                     !/*ur4c      216*/
      rtc(krtc+5,iex)=rtc(krtc+4,iex)                                           !/*ur4c      217*/
      return                                                                    !/*ur4c      218*/
      end                                                                       !/*ur4c      219*/
