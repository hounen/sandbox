!#ident avrwwg.F from deck aa4c                                                 !/*aa4c        1*/
      subroutine avrwwg(df)                                                     !/*aa4c        2*/
!        mesh-based weight window generator.                                    !/*aa4c        3*/
!        df = distance traveled by the particle between events.                 !/*aa4c        4*/
      use global_data                                                           !/*aax         1*/
!                                                                               !/*aax         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*aax         3*/
      implicit integer(kindi) (i-n)                                             !/*aax         4*/
!                                                                               !/*aax         5*/
      dimension mi(3),nf(3)                                                     !/*aa4c        6*/
      dimension dc(3),dd(3),dr(3),cc(3),vc(3),xc(3),yc(3)                       !/*aa4c        7*/
!                                                                               !/*aa4c        8*/
      if(ngww(ipt).eq.0)return                                                  !/*aa4c        9*/
!                                                                               !/*aa4c       10*/
!        calculate mesh direction and location parameters.                      !/*aa4c       11*/
      if(nwgeoa.gt.2)go to 60                                                   !/*aa4c       12*/
      di=1.e-8*df                                                               !/*aa4c2       4*/
      do 10 i=1,3                                                               !/*aa4c       14*/
      dc(i)=gpblcm(i)                                                           !/*aa4c       15*/
      vc(i)=gpblcm(i+3)                                                         !/*aa4c       16*/
      if(lev.ne.0)dc(i)=udt(i,0)                                                !/*aa4c       17*/
      if(lev.ne.0)vc(i)=udt(i+3,0)                                              !/*aa4c       18*/
   10 dd(i)=vc(i)*df                                                            !/*aa4c       19*/
      if(nwgeoa.eq.2)go to 20                                                   !/*aa4c       20*/
      call avrxyz(dc,mi,df,ie)                                                  !/*aax         6*/
      if(ie.eq.1)return                                                         !/*aax         7*/
      go to 60                                                                  !/*aa4c       22*/
!                                                                               !/*aa4c       23*/
!        put particle in cylindrical mesh coordinate system.                    !/*aa4c       24*/
!        calculate r; calculate theta using direction if r=0.                   !/*aa4c       25*/
   20 do 30 i=1,3                                                               !/*aa4c       26*/
      xc(i)=dc(i)-wwma(i+3)                                                     !/*aa4c       27*/
   30 yc(i)=vc(i)                                                               !/*aa4c       28*/
      dc(1)=dotpro(wwma(16),xc)                                                 !/*aa4c       29*/
      dc(3)=dotpro(wwma(20),xc)                                                 !/*aa4c       30*/
      dc(2)=dotpro(wwma(24),xc)                                                 !/*aa4c       31*/
      vc(1)=dotpro(wwma(16),yc)                                                 !/*aa4c       32*/
      vc(3)=dotpro(wwma(20),yc)                                                 !/*aa4c       33*/
      vc(2)=dotpro(wwma(24),yc)                                                 !/*aa4c       34*/
      v=vc(1)**2+vc(2)**2                                                       !/*aa4c       35*/
      cc(1)=sqrt(dc(1)**2+dc(2)**2)                                             !/*aa4c       36*/
      cc(2)=dc(3)                                                               !/*aa4c       37*/
      cc(3)=0.                                                                  !/*aa4c       38*/
      if(cc(1).eq.0.)go to 40                                                   !/*aa4c       39*/
      cc(3)=acos(max(-one,min(one,dc(1)/cc(1))))                                !/*aa4c       40*/
      if(dc(2).lt.0.)cc(3)=pie+pie-cc(3)                                        !/*aa4c2       5*/
      go to 50                                                                  !/*aa4c       42*/
   40 if(v.eq.0.)go to 50                                                       !/*aa4c       43*/
      cc(3)=acos(max(-one,min(one,vc(1)/sqrt(v))))                              !/*aa4c       44*/
      if(vc(2).lt.0.)cc(3)=pie+pie-cc(3)                                        !/*aa4c2       6*/
   50 call avrxyz(cc,mi,df,ie)                                                  !/*aax         8*/
      if(ie.eq.1)return                                                         !/*aax         9*/
!                                                                               !/*aa4c       47*/
!        calculate energy group bin index, ic.                                  !/*aa4c       48*/
   60 ib=0                                                                      !/*aa4c       49*/
      ic=ngww(ipt)                                                              !/*aa4c       50*/
   70 if(ic-ib.eq.1)go to 90                                                    !/*aa4c       51*/
      ih=(ic+ib)/2                                                              !/*aa4c       52*/
      if(erg.lt.ewwg(lewg+ih+mgww(ipt)))go to 80                                !/*aa4c       53*/
      ib=ih                                                                     !/*aa4c       54*/
      go to 70                                                                  !/*aa4c       55*/
   80 ic=ih                                                                     !/*aa4c       56*/
      go to 70                                                                  !/*aa4c       57*/
!                                                                               !/*aa4c       58*/
!        add nmc count for calls from startp for detector (f5) tallies          !/*aa4c       59*/
!        to avoid floating point errors on some platforms.                      !/*aa4c       60*/
   90 if(df.ge.0..and.nwgeoa.lt.3)go to 110                                     !/*aa4c       61*/
      l=icl                                                                     !/*aa4c       62*/
      if(nwgeoa.lt.3)l=1+(mi(1)-2)                                              !/*aa4c       63*/
     & +nint(wwma(1))*(mi(2)-2)+nint(wwma(1)*wwma(2))*(mi(3)-2)                 !/*aa4c       64*/
      i1=l+nwwma*(ic-1+mgww(ipt))                                               !/*aa4c       65*/
      i2=l+nwwma*(ipt-1+mgww(mipt+1))                                           !/*aa4c       66*/
      if(iafg(kifg+i1).gt.0)go to 100                                           !/*aa4c       67*/
      nmc=nmc+1                                                                 !/*aa4c       68*/
      nmco=nmc                                                                  !/*aa4c       69*/
      nmcp(knmc+1,nmc)=ipt                                                      !/*aa4c       70*/
      nmcp(knmc+2,nmc)=ic                                                       !/*aa4c       71*/
      nmcp(knmc+3,nmc)=l                                                        !/*aa4c       72*/
      nmcp(knmc+4,nmc)=0                                                        !/*aa4c       73*/
      wwfa(kwfa+i1)=wwfa(kwfa+i1)+gpblcm(8)                                     !/*aa4c       74*/
      if(iafg(kifg+i2).gt.0)go to 100                                           !/*aa4c       75*/
      nmcp(knmc+4,nmc)=1                                                        !/*aa4c       76*/
      wwfa(kwfa+i2)=wwfa(kwfa+i2)+gpblcm(8)                                     !/*aa4c       77*/
  100 iafg(kifg+i1)=1                                                           !/*aa4c       78*/
      iafg(kifg+i2)=1                                                           !/*aa4c       79*/
      return                                                                    !/*aa4c       80*/
!                                                                               !/*aa4c       81*/
!        do a complete ray trace up to a distance df. do not include            !/*aa4c       82*/
!        mesh cell when di=df. include particle which transports to             !/*aa4c       83*/
!        the surface as entering on next trace (will only occur when            !/*aa4c       84*/
!        the mesh boundary is coincident with a problem boundary.) this         !/*aa4c       85*/
!        strategy is consistent when transporting in all directions             !/*aa4c       86*/
!        (backwards and forwards.)                                              !/*aa4c       87*/
  110 l=1+(mi(1)-2)+nint(wwma(1))*(mi(2)-2)+nint(wwma(1)*wwma(2))*              !/*aa4c       88*/
     & (mi(3)-2)                                                                !/*aa4c       89*/
      i1=l+nwwma*(ic-1+mgww(ipt))                                               !/*aa4c       90*/
      i2=l+nwwma*(ipt-1+mgww(mipt+1))                                           !/*aa4c       91*/
!                                                                               !/*aa4c       92*/
!        sum weights here. calculate nmcp() showing the mesh cells and          !/*aa4c       93*/
!        energy bin passed through by the track. check flag for repeated        !/*aa4c       94*/
!        entries by same track. update flags for reentering particles.          !/*aa4c       95*/
      if(iafg(kifg+i1).ne.0)go to 120                                           !/*aa4c       96*/
      nmc=nmc+1                                                                 !/*aa4c       97*/
      nmco=nmc                                                                  !/*aa4c       98*/
      nmcp(knmc+1,nmc)=ipt                                                      !/*aa4c       99*/
      nmcp(knmc+2,nmc)=ic                                                       !/*aa4c      100*/
      nmcp(knmc+3,nmc)=l                                                        !/*aa4c      101*/
      nmcp(knmc+4,nmc)=0                                                        !/*aa4c      102*/
      wwfa(kwfa+i1)=wwfa(kwfa+i1)+gpblcm(8)                                     !/*aa4c      103*/
      if(iafg(kifg+i2).ne.0)go to 120                                           !/*aa4c      104*/
      nmcp(knmc+4,nmc)=1                                                        !/*aa4c      105*/
      wwfa(kwfa+i2)=wwfa(kwfa+i2)+gpblcm(8)                                     !/*aa4c      106*/
  120 iafg(kifg+i1)=1                                                           !/*aa4c      107*/
      iafg(kifg+i2)=1                                                           !/*aa4c      108*/
!                                                                               !/*aa4c      109*/
!        do ray trace to next mesh surface.                                     !/*aa4c      110*/
!        xyz ray trace:                                                         !/*aa4c      111*/
      if(nwgeoa.ne.1)go to 160                                                  !/*aa4c      112*/
      do 130 i=1,3                                                              !/*aa4c      113*/
      na=lasm+i                                                                 !/*aa4c      114*/
      dr(i)=huge                                                                !/*aa4c      115*/
      if(dd(i).gt.0.)dr(i)=(asm(na,mi(i))-dc(i))/vc(i)                          !/*aa4c      116*/
  130 if(dd(i).lt.0.)dr(i)=(asm(na,mi(i)-1)-dc(i))/vc(i)                        !/*aa4c      117*/
      d=min(dr(1),dr(2),dr(3))                                                  !/*aa4c      118*/
      di=d+di                                                                   !/*aa4c      119*/
      do 140 i=1,3                                                              !/*aa4c      120*/
      if(d.eq.dr(i).and.dd(i).gt.0.)mi(i)=mi(i)+1                               !/*aa4c      121*/
      if(d.eq.dr(i).and.dd(i).lt.0.)mi(i)=mi(i)-1                               !/*aa4c      122*/
      dc(i)=vc(i)*d+dc(i)                                                       !/*aa4c      123*/
  140 if((mi(i).eq.1.or.mi(i).gt.ism(i)).and.di.lt.df)go to 150                 !/*aa4c      124*/
      if(di.lt.df)go to 110                                                     !/*aa4c      125*/
      return                                                                    !/*aa4c      126*/
  150 ns=min(mi(i),ism(i))                                                      !/*aa4c      127*/
      call errprn(1,nesm,2,zero+ns,zero+ncl(lncl+icl),'msh','cel',              !/*aa4c      128*/
     & 'particle escaped wwg mesh in '//avrm(i)//' direction.')                 !/*aa4c      129*/
      if(nesm.gt.1)return                                                       !/*aa4c2       7*/
#ifdef MULTT                                                                    /*!aaxk        1*/
      call thread_lockon(jlock)                                                 !/*aaxk        2*/
#endif                                                                          /*!aaxk        3*/
      write(iuo,'(8h x,y,z =,1p3e15.4)')xxx,yyy,zzz                             !/*aa4c2       9*/
#ifdef MULTT                                                                    /*!aaxk        4*/
      call thread_lockoff(jlock)                                                !/*aaxk        5*/
#endif                                                                          /*!aaxk        6*/
      return                                                                    !/*aa4c      131*/
!                                                                               !/*aa4c      132*/
!        cylindrical geometry, rzt ray trace:                                   !/*aa4c      133*/
!        radial (r)                                                             !/*aa4c      134*/
  160 na=lasm+1                                                                 !/*aa4c      135*/
      rv=vc(1)*dc(1)+vc(2)*dc(2)                                                !/*aa4c      136*/
      dr(1)=huge                                                                !/*aa4c      137*/
      if(v.eq.0.)go to 180                                                      !/*aa4c      138*/
      rs=dc(1)**2+dc(2)**2                                                      !/*aa4c      139*/
      a=rv/v                                                                    !/*aa4c      140*/
      b=a**2-rs/v                                                               !/*aa4c      141*/
      if(rv.ge.0.)go to 170                                                     !/*aa4c      142*/
      sq=b+asm(na,mi(1)-1)**2/v                                                 !/*aa4c      143*/
      if(sq.lt.-1.e-28)go to 170                                                !/*aa4c      144*/
      dr(1)=-a-sqrt(abs(sq))                                                    !/*aa4c      145*/
      nf(1)=-1                                                                  !/*aa4c      146*/
      go to 180                                                                 !/*aa4c      147*/
  170 sq=b+asm(na,mi(1))**2/v                                                   !/*aa4c      148*/
      dr(1)=-a+sqrt(abs(sq))                                                    !/*aax        12*/
      nf(1)=1                                                                   !/*aa4c      150*/
!                                                                               !/*aa4c      151*/
!        axial (z)                                                              !/*aa4c      152*/
  180 na=lasm+2                                                                 !/*aa4c      153*/
      dr(2)=huge                                                                !/*aa4c      154*/
      if(vc(3).eq.0.)go to 200                                                  !/*aa4c      155*/
      if(vc(3).lt.0.)go to 190                                                  !/*aa4c      156*/
      dr(2)=(asm(na,mi(2))-dc(3))/vc(3)                                         !/*aa4c      157*/
      nf(2)=1                                                                   !/*aa4c      158*/
      go to 200                                                                 !/*aa4c      159*/
  190 dr(2)=(asm(na,mi(2)-1)-dc(3))/vc(3)                                       !/*aa4c      160*/
      nf(2)=-1                                                                  !/*aa4c      161*/
!                                                                               !/*aa4c      162*/
!        azimuthal (theta)                                                      !/*aa4c      163*/
!        if single azimuthal bin (ism(3)=2), point is always in it.             !/*aa4c2      11*/
  200 dr(3)=huge                                                                !/*aa4c2      12*/
      if(ism(3).eq.2)go to 220                                                  !/*aa4c2      13*/
      na=lasm+3                                                                 !/*aa4c2      14*/
      rp=-vc(1)*dc(2)+vc(2)*dc(1)                                               !/*aa4c2      15*/
      if(rp.ge.0.)go to 210                                                     !/*aa4c      167*/
      t=asm(na,mi(3)-1)                                                         !/*aa4c      168*/
      st=sin(t)                                                                 !/*aa4c      169*/
      ct=cos(t)                                                                 !/*aa4c      170*/
      vn=-vc(1)*st+vc(2)*ct                                                     !/*aa4c      171*/
      if(vn.ge.0.)go to 220                                                     !/*aa4c      172*/
      dr(3)=(st*dc(1)-ct*dc(2))/vn                                              !/*aa4c      173*/
      nf(3)=-1                                                                  !/*aa4c      174*/
      if(dr(3).le.0.)dr(3)=huge                                                 !/*aa4c      175*/
      go to 220                                                                 !/*aa4c      176*/
  210 t=asm(na,mi(3))                                                           !/*aa4c      177*/
      st=sin(t)                                                                 !/*aa4c      178*/
      ct=cos(t)                                                                 !/*aa4c      179*/
      vn=-vc(1)*st+vc(2)*ct                                                     !/*aa4c      180*/
      if(vn.le.0.)go to 220                                                     !/*aa4c      181*/
      dr(3)=(st*dc(1)-ct*dc(2))/vn                                              !/*aa4c      182*/
      nf(3)=1                                                                   !/*aa4c      183*/
      if(dr(3).le.0.)dr(3)=huge                                                 !/*aa4c      184*/
!                                                                               !/*aa4c      185*/
  220 d=min(dr(1),dr(2),dr(3))                                                  !/*aa4c      186*/
      di=di+d                                                                   !/*aa4c      187*/
      do 270 i=1,3                                                              !/*aa4c      188*/
      if(d.eq.dr(i))mi(i)=mi(i)+nf(i)                                           !/*aa4c      189*/
      dc(i)=vc(i)*d+dc(i)                                                       !/*aa4c      190*/
      if(mi(i).eq.1)go to(230,240,250)i                                         !/*aa4c      191*/
      if(mi(i).gt.ism(i))go to(240,240,260)i                                    !/*aa4c      192*/
      go to 270                                                                 !/*aa4c      193*/
  230 dc(1)=0.                                                                  !/*aa4c      194*/
      dc(2)=0.                                                                  !/*aa4c      195*/
      cc(1)=0.                                                                  !/*aa4c      196*/
      cc(2)=dc(3)                                                               !/*aa4c      197*/
      cc(3)=acos(max(-one,min(one,vc(1)/sqrt(v))))                              !/*aa4c      198*/
      if(vc(2).lt.0.)cc(3)=pie+pie-cc(3)                                        !/*aa4c2      16*/
      call avrxyz(cc,mi,df,ie)                                                  !/*aax        13*/
      go to 270                                                                 !/*aa4c      201*/
  240 if(di.lt.df)go to 280                                                     !/*aa4c2      17*/
      go to 270                                                                 !/*aa4c      203*/
  250 mi(i)=ism(i)                                                              !/*aa4c      204*/
      go to 270                                                                 !/*aa4c      205*/
  260 mi(i)=2                                                                   !/*aa4c      206*/
  270 continue                                                                  !/*aa4c      207*/
      if(di.lt.df)go to 110                                                     !/*aa4c      208*/
      return                                                                    !/*aa4c      209*/
  280 ns=mi(i)-.5*(nf(i)+1)                                                     !/*aa4c      210*/
      call errprn(1,nesm,2,zero+ns,zero+ncl(lncl+icl),'msh','cel',              !/*aa4c      211*/
     & 'particle escaped wwg mesh in '//avrm(3+i)//' direction.')               !/*aa4c      212*/
      if(nesm.gt.1)return                                                       !/*aa4c2      18*/
#ifdef MULTT                                                                    /*!aaxk        7*/
      call thread_lockon(jlock)                                                 !/*aaxk        8*/
#endif                                                                          /*!aaxk        9*/
      write(iuo,'(8h x,y,z =,1p3e15.4)')xxx,yyy,zzz                             !/*aa4c2      20*/
#ifdef MULTT                                                                    /*!aaxk       10*/
      call thread_lockoff(jlock)                                                !/*aaxk       11*/
#endif                                                                          /*!aaxk       12*/
      return                                                                    !/*aa4c      214*/
      end                                                                       !/*aa4c      215*/
