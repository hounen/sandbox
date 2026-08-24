!#ident pltsup.F from deck pup4c2                                               !/*pup4c2      1*/
      subroutine pltsup(nq)                                                     !/*pup4c2      3*/
#ifdef PLOT                                                                     /*!pupx        1*/
!        interactive geometry plotting and setup.                               !/*pup4c2      4*/
!        nq = -1 first time through.                                            !/*pup4c2      5*/
!        nq = plotg branching flag: 1/0/2=redraw/prompt/end                     !/*pup4c2      6*/
      use global_data                                                           !/*pupx        2*/
      use plot_data                                                             !/*pupx        3*/
!                                                                               !/*pupx        4*/
      implicit real(kindr) (a-h,o-z)                                            !/*pupx        5*/
      implicit integer(kindi) (i-n)                                             !/*pupx        6*/
!                                                                               !/*pupx        7*/
      dimension ex(2),np(mclb),nn(mclb)                                         !/*pup4c2      9*/
      character ha*80,hb*10,hc(mclb)*4,hl*29                                    !/*pup4c2     10*/
      data hc/'cel','imp','rho','den','vol','fcl','mas','pwt','mat',            !/*pup4c2     11*/
     & 'tmp','wwn','ext','pd','dxc','u','lat','fill','nonu','pac'/              !/*pup4c2     12*/
!        np = 0/1 = hc(i) cannot/can have particle type, ie, imp:p              !/*pup4c2     13*/
!        nn = 0/1 = hc(i) cannot/can have number, ie, pd15                      !/*pup4c2     14*/
      data np/0,1,3*0,1,4*0,1,1,0,1,4*0,1/,nn/9*0,1,1,0,1,1,4*0,1/              !/*pup4c2     15*/
      real et(2),s1,s2,t1,t2,xh,xy(2,2)                                         !/*pup4c2     16*/
!                                                                               !/*pup4c2     17*/
! ************************** initialize ********************************        !/*pup4c2     18*/
!                                                                               !/*pup4c2     19*/
!        first time through initialize saved variables.                         !/*pup4c2     20*/
      if(nq.ge.0.and.zst(lzst+1).eq.0..and.mod(meshpl,2).ne.0)go to 910         !/*pup4c2     21*/
      if(nq.ge.0)go to 20                                                       !/*pup4c2     22*/
      if(iterm.eq.0)nq=0                                                        !/*pup4c2     23*/
      if(iterm.eq.0)return                                                      !/*pup4c2     24*/
      orsav(1)=0.                                                               !/*pup4c2     25*/
      orsav(2)=0.                                                               !/*pup4c2     26*/
      orsav(3)=0.                                                               !/*pup4c2     27*/
      exsav(1)=100.                                                             !/*pup4c2     28*/
      exsav(2)=100.                                                             !/*pup4c2     29*/
      idne(lide+idne(lide+2)+4)=-1                                              !/*pup4c2     30*/
      do 10 i=1,mlja                                                            !/*pup4c2     31*/
   10 if(idna(lida+i).ne.0)idne(lide+idne(lide+2)+4)=0                          !/*pup4c2     32*/
!                                                                               !/*pup4c2     33*/
!        prepare to draw at the terminal and/or on the metafile.                !/*pup4c2     34*/
      call gxskip                                                               !/*pup4c2     35*/
      if(iterm.ne.0)call gxon(1)                                                !/*pup4c2     36*/
      if(iterm.eq.0)call gxoff(1)                                               !/*pup4c2     37*/
      i=iterm                                                                   !/*pup4c2     38*/
      if(jgf.ne.0.or.i.eq.0)call gxon(2)                                        !/*pup4c2     39*/
      if(jgf.eq.0.and.i.ne.0)call gxoff(2)                                      !/*pup4c2     40*/
      if(jgxo(1)+jgxo(2).eq.0)return                                            !/*pup4c2     41*/
!                                                                               !/*pup4c2     42*/
!        specify the window and the character expansion factor.                 !/*pup4c2     43*/
      et(1)=extent(1)*(1.+.1*(1-jvp))                                           !/*pup4c2     44*/
      et(2)=extent(2)*(1.+.1*(1-jvp))                                           !/*pup4c2     45*/
      call gswn(1,-et(1)*(5-2*jvp)/3.,et(1),-et(2),et(2))                       !/*pup4c2     46*/
      wnvp(1)=et(1)*(8-2*jvp)/3.                                                !/*pup4c2     47*/
      wnvp(2)=et(2)*2.                                                          !/*pup4c2     48*/
      call gschxp(et(1)/et(2))                                                  !/*pup4c2     49*/
!                                                                               !/*pup4c2     50*/
!        set character size and setup colors.                                   !/*pup4c2     51*/
      if(mcolor.gt.0)call gsfais(1)                                             !/*pup4c2     52*/
      call gschh(real(sch*extent(2)))                                           !/*pup4c2     53*/
      if(mcolor.gt.2)call gsplci(kcolor(ncolor+1))                              !/*pup4c2     54*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+1))                              !/*pup4c2     55*/
!                                                                               !/*pup4c2     56*/
!        more initialization.                                                   !/*pup4c2     57*/
!        iq = how many times picture has been clicked.                          !/*pup4c2     58*/
!        kq = last left menu command index.                                     !/*pup4c2     59*/
!        lq = last top/bottom menu command index.                               !/*pup4c2     60*/
!        ir = rotate flag.                                                      !/*pup4c2     61*/
   20 iq=0                                                                      !/*pup4c2     62*/
      kq=0                                                                      !/*pup4c2     63*/
      lq=0                                                                      !/*pup4c2     64*/
      ir=0                                                                      !/*pup4c2     65*/
      ex(1)=extent(1)                                                           !/*pup4c2     66*/
      ex(2)=extent(2)                                                           !/*pup4c2     67*/
      xh=-1.82*ex(1)                                                            !/*pup4c2     68*/
!                                                                               !/*pup4c2     69*/
!        turn off metafile plot -- do not plot interactive legends.             !/*pup4c2     70*/
!        test on jgxa(2), not jgf, because jgf=0 set in plotg.                  !/*pup4c2     71*/
      i=iterm                                                                   !/*pup4c2     72*/
      if(jgxa(2).eq.0.or.i.eq.0)go to 50                                        !/*pup4c2     73*/
      call gclrwk(2,1)                                                          !/*pup4c2     74*/
      call guwk(2,1)                                                            !/*pup4c2     75*/
      call gxoff(2)                                                             !/*pup4c2     76*/
      jgf=0                                                                     !/*pup4c2     77*/
!                                                                               !/*pup4c2     78*/
!        set up top and bottom menus.                                           !/*pup4c2     79*/
   50 if(mcolor.gt.2)call gstxci(kcolor(ncolor+3))                              !/*pup4c2     80*/
      if(mcolor.gt.2)call gsplci(kcolor(ncolor+3))                              !/*pup4c2     81*/
      call gschh(real(.039*ex(2)))                                              !/*pup4c2     82*/
      call gtx(real(-.95*ex(1)),real(1.02*ex(2)),'UP')                          !/*pup4c2     83*/
      call gtx(real(-.80*ex(1)),real(1.02*ex(2)),'RT')                          !/*pup4c2     84*/
      call gtx(real(-.65*ex(1)),real(1.02*ex(2)),'DN')                          !/*pup4c2     85*/
      call gtx(real(-.50*ex(1)),real(1.02*ex(2)),'LF')                          !/*pup4c2     86*/
      call gtx(real(-.30*ex(1)),real(1.02*ex(2)),'Origin')                      !/*pup4c2     87*/
      call gtx(0.,real(1.02*ex(2)),'.1')                                        !/*pup4c2     88*/
      call gtx(real(.23*ex(1)),real(1.02*ex(2)),' .2 ')                         !/*pup4c2     89*/
      call gtx(real(.45*ex(1)),real(1.02*ex(2)),'Zoom')                         !/*pup4c2     90*/
      call gtx(real(.68*ex(1)),real(1.02*ex(2)),' 5. ')                         !/*pup4c2     91*/
      call gtx(real(.95*ex(1)),real(1.02*ex(2)),'10')                           !/*pup4c2     92*/
      call gtx(real(-0.9*ex(1)),real(-1.06*ex(2)),'      ')                     !/*pup4c2     93*/
      call gtx(real(-0.3*ex(1)),real(-1.06*ex(2)),'Redraw')                     !/*pup4c2     94*/
      call gtx(real( 0.2*ex(1)),real(-1.06*ex(2)),'Plot>')                      !/*pup4c2     95*/
      call gtx(real( 0.8*ex(1)),real(-1.06*ex(2)),'End')                        !/*pup4c2     96*/
!                                                                               !/*pup4c2     97*/
!        set up right margin:  cards to edit.                                   !/*pup4c2     98*/
      s1=ex(1)                                                                  !/*pup4c2     99*/
      do 60 i=1,mclb                                                            !/*pupx        8*/
      t1=(1.-0.08*i)*ex(2)                                                      !/*pupx        9*/
   60 call gtx(s1,t1,hc(i))                                                     !/*pup4c2    103*/
      call gtx(s1,real(-.90*ex(2)),'PAR')                                       !/*pupx       10*/
      call gtx(s1,real(-.98*ex(2)),'N')                                         !/*pup4c2    105*/
!                                                                               !/*pup4c2    106*/
!        initialize left menu options.                                          !/*pup4c2    107*/
!          jv = card index in hc(jv)                                            !/*pup4c2    108*/
!          ip = particle type                                                   !/*pup4c2    109*/
!          nv = number on card (ie, pd15)                                       !/*pup4c2    110*/
!          c1,c2 = initial character sizes for cell/surface labels.             !/*pup4c2    111*/
      c1=sclabl(1)                                                              !/*pup4c2    112*/
      c2=sclabl(2)                                                              !/*pup4c2    113*/
      if(c1.eq.0.)c1=1.                                                         !/*pup4c2    114*/
      if(c2.eq.0.)c2=1.                                                         !/*pup4c2    115*/
      jv=nint(sclabl(3))                                                        !/*pup4c2    116*/
      if(jv.le.1.or.jv.gt.mclb)jv=1                                             !/*pup4c2    117*/
      ip=min(max(nint(sclabl(4)/1000),1),mipt)                                  !/*pupk        1*/
      if(kpt(ip).eq.0)ip=nips                                                   !/*pup4c2    119*/
      nv=max(1,mod(int(sclabl(4)),1000))                                        !/*pup4c2    120*/
      call pltloc(0.,0.)                                                        !/*pup4c2    121*/
!                                                                               !/*pup4c2    122*/
!        set up left menu geometry plot option buttons.                         !/*pup4c2    123*/
      call gtx(real(xh),real(-.44*ex(2)),'CURSOR')                              !/*pup4c2    124*/
      write(hl,'(6hSCALES,i2)')jscal                                            !/*pupk        2*/
      call gtx(real(xh+.32*ex(1)),real(-.44*ex(2)),hl)                          !/*pupk        3*/
      if(meshpl.eq.0)hl='No Lines'                                              !/*pup4c2    126*/
      if(mod(meshpl,2).ne.0)hl='CellLine'                                       !/*pup4c2    127*/
      if(mod(meshpl/2,2).ne.0)hl=' WW MESH'                                     !/*pup4c2    128*/
      if(meshpl.ge.3)hl=' WW+Cell'                                              !/*pup4c2    129*/
      call gtx(real(xh+.56*ex(1)),real(-.44*ex(2)),hl)                          !/*pup4c2    130*/
      call gtx(real(xh+.32*ex(1)),real(-.52*ex(2)),'ROTATE')                    !/*pup4c2    131*/
      call gtx(xh,real(-.68*ex(2)),'XY')                                        !/*pup4c2    134*/
      call gtx(real(xh+.32*ex(1)),real(-.68*ex(2)),'YZ')                        !/*pup4c2    135*/
      call gtx(real(xh+.64*ex(1)),real(-.68*ex(2)),'ZX')                        !/*pup4c2    136*/
      call gtx(xh,real(-.52*ex(2)),'PostScript')                                !/*pup4c2    137*/
      j=min(max(nint(sclabl(5)),1),mclb)                                        !/*pupk        4*/
      i=min(max(nint(sclabl(6)/1000),1),mipt)                                   !/*pupk        5*/
      if(kpt(i).eq.0)i=nips                                                     !/*pupk        6*/
      n=max(1,mod(int(sclabl(6)),1000))                                         !/*pupk        7*/
      call pltdat(1,j,i,n,ie,ar,an,ha)                                          !/*pupk        8*/
      if(mcolor.le.0)write(hl,'(15hCOLOR       off)')                           !/*pupk        9*/
      if(mcolor.gt.0)write(hl,'(5hCOLOR,a10)')ha(1:10)                          !/*pupk       10*/
      call gtx(xh,real(-.60*ex(2)),hl(1:15))                                    !/*pupk       11*/
      hl='LEVEL'                                                                !/*pup4c2    139*/
      if(levplt.ge.0)write(hl,'(5hLEVEL,i2)')levplt                             !/*pup4c2    140*/
      if(junf.ne.0)call gtx(real(xh+.64*ex(1)),real(-.60*ex(2)),hl)             !/*pup4c2    141*/
      call gtx(xh,real(-.76*ex(2)),'LABEL')                                     !/*pup4c2    142*/
      hl='sur'                                                                  !/*pup4c2    143*/
      if(sclabl(1).eq.0.)hl='off'                                               !/*pup4c2    144*/
      call gtx(real(xh+.32*ex(1)),real(-.76*ex(2)),hl)                          !/*pup4c2    145*/
      ha='       off'                                                           !/*pup4c2    146*/
      if(sclabl(2).ne.0.)call pltdat(1,jv,ip,nv,ie,ar,an,ha)                    !/*pupk       12*/
      call gtx(real(xh+.44*ex(1)),real(-.76*ex(2)),ha)                          !/*pup4c2    148*/
      if(idne(lide+idne(lide+2)+4).eq.0)                                        !/*pup4c2    149*/
     & call gtx(xh,real(-.84*ex(2)),'MBODY  on')                                !/*pup4c2    150*/
      if(idne(lide+idne(lide+2)+4).gt.0)                                        !/*pup4c2    151*/
     & call gtx(xh,real(-.84*ex(2)),'MBODY off')                                !/*pup4c2    152*/
!                                                                               !/*pup4c2    153*/
! ******************************* start ********************************        !/*pup4c2    154*/
!                                                                               !/*pup4c2    155*/
!        set up left cell status options.                                       !/*pup4c2    156*/
   70 if(mcolor.gt.2)call gstxci(kcolor(ncolor+3))                              !/*pup4c2    157*/
      call pltdat(1,jv,ip,nv,ie,ar,an,ha)                                       !/*pupk       13*/
      write(hl,'(4hEdit,a10)')ha                                                !/*pup4c2    159*/
      ie=0                                                                      !/*pup4c2    160*/
      call pltdat(2,jv,ip,nv,ie,ar,an,ha)                                       !/*pupk       14*/
      if(ie.eq.0)call fshort(ar,hb,n)                                           !/*pup4c2    162*/
      if(ie.eq.0)hl(24-n+1:24)=hb(1:n)                                          !/*pup4c2    163*/
      if(ie.eq.1)hl(18:24)='ILLEGAL'                                            !/*pup4c2    164*/
      if(ie.eq.-1)hl(18:24)=' -NULL-'                                           !/*pup4c2    165*/
      call gtx(xh,real(-.20*ex(2)),hl)                                          !/*pup4c2    166*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+1))                              !/*pup4c2    167*/
      call gschh(real(.033*ex(2)))                                              !/*pup4c2    168*/
      write(ha,'(25x,4hCell,i5)')ncl(lncl+iap)                                  !/*pup4c2    169*/
      call gtx(xh,real(-.28*ex(2)),ha)                                          !/*pup4c2    170*/
      write(ha,'(5hxyz =,f9.2,1h,,f9.2,1h,,f9.2)')xxx,yyy,zzz                   !/*pup4c2    171*/
      call gtx(xh,real(-.36*ex(2)),ha)                                          !/*pup4c2    172*/
!                                                                               !/*pup4c2    173*/
!        restore dialog box.                                                    !/*pup4c2    174*/
      f0=0.                                                                     !/*pup4c2    175*/
      call gschh(real(.039*ex(2)))                                              !/*pup4c2    176*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+7))                              !/*pup4c2    177*/
      hl='                            >'                                        !/*pup4c2    178*/
      call gtx(xh,real(-.92*ex(2)),hl)                                          !/*pup4c2    179*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+2))                              !/*pup4c2    180*/
      if(nq.lt.0)                                                               !/*pup4c2    181*/
     & call gtx(real(-.7*ex(1)),0.,'Click here for default plot')               !/*pup4c2    182*/
      write(hl,'(29hClick here or picture or menu)')                            !/*pup4c2    183*/
      call gtx(xh,real(-ex(2)),hl)                                              !/*pup4c2    184*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+3))                              !/*pup4c2    185*/
!                                                                               !/*pup4c2    186*/
!        get click location.                                                    !/*pup4c2    187*/
   90 call ginlc(1,1,1,0.,0.,1,0.,0.,0.,0.,1,' ')                               !/*pup4c2    188*/
      call grqlc(1,0,i,j,s1,t1)                                                 !/*pup4c2    189*/
      if(s1.gt.ex(1))go to 350                                                  !/*pup4c2    190*/
      if(s1.lt.-ex(1))go to 400                                                 !/*pup4c2    191*/
      if(t1.lt.-ex(2))go to 200                                                 !/*pup4c2    192*/
      if(t1.gt.ex(2))go to 100                                                  !/*pup4c2    193*/
      if(nq.lt.0)go to 900                                                      !/*pup4c2    194*/
      go to 800                                                                 !/*pup4c2    195*/
!                                                                               !/*pup4c2    196*/
! ******************* click was in top menu ****************************        !/*pup4c2    197*/
!                                                                               !/*pup4c2    198*/
!        change origin or extent by <, >, v, <, origin, or zoom.                !/*pup4c2    199*/
  100 fc=1.                                                                     !/*pup4c2    200*/
      orsav(1)=origin(1)                                                        !/*pup4c2    201*/
      orsav(2)=origin(2)                                                        !/*pup4c2    202*/
      orsav(3)=origin(3)                                                        !/*pup4c2    203*/
      exsav(1)=extent(1)                                                        !/*pup4c2    204*/
      exsav(2)=extent(2)                                                        !/*pup4c2    205*/
  110 if(s1.ge.0.)go to 130                                                     !/*pup4c2    206*/
      if(s1.gt.-.4*ex(1))go to 120                                              !/*pup4c2    207*/
!                                                                               !/*pup4c2    208*/
!        up/right/down/left:  go to new frame.                                  !/*pup4c2    209*/
      j=max(1,min(4,int((1.15+s1/ex(1))/.15)))                                  !/*pup4c2    210*/
      s1=0.                                                                     !/*pup4c2    211*/
      t1=0.                                                                     !/*pup4c2    212*/
      if(j.eq.1)t1=2.*ex(2)                                                     !/*pup4c2    213*/
      if(j.eq.2)s1=2.*ex(1)                                                     !/*pup4c2    214*/
      if(j.eq.3)t1=-2.*ex(2)                                                    !/*pup4c2    215*/
      if(j.eq.4)s1=-2.*ex(1)                                                    !/*pup4c2    216*/
      go to 140                                                                 !/*pup4c2    217*/
!                                                                               !/*pup4c2    218*/
!        origin.  set flag and get new origin.                                  !/*pup4c2    219*/
  120 lq=10                                                                     !/*pup4c2    220*/
      go to 90                                                                  !/*pup4c2    221*/
!                                                                               !/*pup4c2    222*/
!        zoom.  get factor, fc.                                                 !/*pup4c2    223*/
  130 lq=11                                                                     !/*pup4c2    224*/
      f=s1/ex(1)                                                                !/*pup4c2    225*/
      if(f.lt..5)fc=max(1,10-int(20.*f))                                        !/*pup4c2    226*/
      if(f.ge..5)fc=1./min(10.+zero,one+int(20.*(f-.5)))                        !/*pup4c2    227*/
      if(f0.eq.fc)go to 160                                                     !/*pup4c2    228*/
      f0=fc                                                                     !/*pup4c2    229*/
      write(hl,'(16hZoom factor = 1/,i1,1hx,10x,1h>)')nint(fc)                  !/*pupx       11*/
      if(fc.gt.9.5)write(hl,'(19hZoom factor = 1/10x,9x,1h>)')                  !/*pup4c2    231*/
      if(fc.lt.1.)write(hl,'(13hZoom factor =,i2,1hx,12x,1h>)')                 !/*pup4c2    232*/
     & nint(1./fc)                                                              !/*pup4c2    233*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+5))                              !/*pup4c2    234*/
      call gtx(xh,real(-.92*ex(2)),hl)                                          !/*pup4c2    235*/
      write(hl,'(29hClick picture or double click)')                            !/*pup4c2    236*/
      call gtx(xh,real(-ex(2)),hl)                                              !/*pup4c2    237*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+3))                              !/*pup4c2    238*/
      go to 90                                                                  !/*pup4c2    239*/
!                                                                               !/*pup4c2    240*/
!        get new origin and extent, then redraw.                                !/*pup4c2    241*/
  140 do 150 i=1,3                                                              !/*pup4c2    242*/
      origin(i)=plmx(i+1,1)+s1*plmx(i+1,2)+t1*plmx(i+1,3)                       !/*pup4c2    243*/
  150 plmx(i+1,1)=origin(i)                                                     !/*pup4c2    244*/
  160 extent(1)=fc*extent(1)                                                    !/*pup4c2    245*/
      extent(2)=fc*extent(2)                                                    !/*pup4c2    246*/
      write(iuc,'(6horigin,1p3e12.4,7h extent,1p2e12.4)')                       !/*pup4c2    247*/
     & (origin(i),i=1,3),extent(1),extent(2)                                    !/*pup4c2    248*/
      go to 900                                                                 !/*pup4c2    249*/
!                                                                               !/*pup4c2    250*/
! ******************* click was in bottom menu *************************        !/*pup4c2    251*/
!                                                                               !/*pup4c2    252*/
!        lq = 1/2/3/4/5/6 = null/null/redraw/plot>/null/end.                    !/*pup4c2    253*/
  200 lq=int(4.+3.*s1/ex(1))                                                    !/*pup4c2    254*/
      go to(90,90,900,910,90,330)lq                                             !/*pup4c2    255*/
!                                                                               !/*pup4c2    256*/
! >>>>>  end.                                                                   !/*pup4c2    257*/
  330 nq=2                                                                      !/*pup4c2    258*/
      call gxskip                                                               !/*pup4c2    259*/
      call gxoff(1)                                                             !/*pup4c2    260*/
      call gxoff(2)                                                             !/*pup4c2    261*/
      write(iuc,'(3hend)')                                                      !/*pup4c2    262*/
      write(iuc,'(3hend)')                                                      !/*pup4c2    263*/
      return                                                                    !/*pup4c2    264*/
!                                                                               !/*pup4c2    265*/
! ******************** click was in right menu *************************        !/*pup4c2    266*/
!                                                                               !/*pup4c2    267*/
! >>>>>  get new indicies of parameters to change.                              !/*pup4c2    268*/
  350 kq=0                                                                      !/*pup4c2    269*/
      t0=.97*ex(2)                                                              !/*pupx       12*/
      j=max(0,int(12.5*(t0-t1)/ex(2)))+1                                        !/*pupx       13*/
      if(j.gt.mclb)go to 360                                                    !/*pupx       14*/
      jv=j                                                                      !/*pup4c2    273*/
      go to 70                                                                  !/*pup4c2    274*/
!                                                                               !/*pup4c2    275*/
!        increment particle type.                                               !/*pup4c2    276*/
  360 if(t1.lt.-.92*ex(2))go to 380                                             !/*pup4c2    277*/
      if(np(jv).eq.0)go to 90                                                   !/*pup4c2    278*/
  370 ip=mod(ip,mipt)+1                                                         !/*pup4c2    279*/
      if(kpt(ip).eq.0)go to 370                                                 !/*pup4c2    280*/
      go to 70                                                                  !/*pup4c2    281*/
!                                                                               !/*pup4c2    282*/
!        increment number.                                                      !/*pup4c2    283*/
  380 if(jv.lt.10.or.jv.gt.11)go to 390                                         !/*pup4c2    284*/
      if(jv.eq.10)n=mxt                                                         !/*pup4c2    285*/
      if(jv.eq.11)n=nww(ip)                                                     !/*pup4c2    286*/
      if(nv.lt.0.or.nv.ge.n)nv=0                                                !/*pup4c2    287*/
      nv=nv+1                                                                   !/*pup4c2    288*/
      go to 70                                                                  !/*pup4c2    289*/
  390 if(nn(jv).eq.0)go to 90                                                   !/*pup4c2    290*/
      call pltscr                                                               !/*pup4c2    291*/
      jc=1                                                                      !/*pup4c2    292*/
      kd=kdarg(klin,jc,0,hb,ar)                                                 !/*pup4c2    293*/
      if(kd.ne.2)go to 390                                                      !/*pup4c2    294*/
      nv=nint(ar)                                                               !/*pup4c2    295*/
      go to 70                                                                  !/*pup4c2    296*/
!                                                                               !/*pup4c2    297*/
! ********************* click was in left menu *************************        !/*pup4c2    298*/
!                                                                               !/*pup4c2    299*/
  400 if(t1.gt..5*ex(2))go to 900                                               !/*pup4c2    300*/
      t0=.10*ex(2)                                                              !/*pup4c2    301*/
      kq=int(12.5*(t0-t1)/ex(2))+1                                              !/*pup4c2    302*/
      if(kq.lt.1)go to 90                                                       !/*pup4c2    303*/
      go to(90,90,90,450,90,90,550,580,630,660,680,710)kq                       !/*pup4c2    304*/
      if(t1.lt.-.88*ex(2))go to 760                                             !/*pup4c2    305*/
!                                                                               !/*pup4c2    306*/
! >>>>>  edit.                                                                  !/*pup4c2    307*/
!        change value of card jv for current cell iap.                          !/*pup4c2    308*/
  450 if(iap.eq.0)go to 70                                                      !/*pup4c2    309*/
      call pltscr                                                               !/*pup4c2    310*/
      jc=1                                                                      !/*pup4c2    311*/
      kd=kdarg(klin,jc,0,hb,ar)                                                 !/*pup4c2    312*/
      if(kd.le.0)go to 450                                                      !/*pup4c2    313*/
      call pltdat(3,jv,ip,nv,ie,ar,an,ha)                                       !/*pupk       15*/
      iq=0                                                                      !/*pup4c2    315*/
      go to 70                                                                  !/*pup4c2    316*/
!                                                                               !/*pup4c2    317*/
! >>>>>  cursor:  get new origin and extent.                                    !/*pup4c2    318*/
  550 if(s1.gt.xh+.28*ex(1))go to 560                                           !/*pup4c2    319*/
      call ginlc(1,1,1,0.,0.,1,0.,0.,0.,0.,1,' ')                               !/*pup4c2    320*/
      call grqlc(1,2,i,j,s1,t1)                                                 !/*pup4c2    321*/
      call ginlc(1,1,1,0.,0.,1,0.,0.,0.,0.,1,' ')                               !/*pup4c2    322*/
      call grqlc(1,3,i,j,s2,t2)                                                 !/*pup4c2    323*/
      extent(1)=max(.01*extent(1),zero+.5*max(abs(s1-s2),abs(t1-t2)))           !/*pup4c2    324*/
      extent(2)=extent(1)                                                       !/*pup4c2    325*/
      s1=.5*(s1+s2)                                                             !/*pup4c2    326*/
      t1=.5*(t1+t2)                                                             !/*pup4c2    327*/
      fc=1.                                                                     !/*pup4c2    328*/
      go to 140                                                                 !/*pup4c2    329*/
!                                                                               !/*pup4c2    330*/
! >>>>>  scales:  0/1/2 = none/edge only/grid.                                  !/*pupk       16*/
  560 if(s1.gt.xh+.56*ex(1))go to 570                                           !/*pupk       17*/
      jscal=mod(jscal+1,3)                                                      !/*pupk       18*/
      write(hl,'(6hSCALES,i2)')jscal                                            !/*pupk       19*/
      call gtx(real(xh+.32*ex(1)),real(-.44*ex(2)),hl)                          !/*pupk       20*/
      write(iuc,'(5hscale,i9)')jscal                                            !/*pupk       21*/
      go to 90                                                                  !/*pup4c2    333*/
!                                                                               !/*pup4c2    334*/
! >>>>>  mesh:  meshpl = 0/1/2/3 = outline none/cells/mesh/both.                !/*pup4c2    335*/
  570 meshpl=meshpl+1                                                           !/*pup4c2    336*/
      if(mod(meshpl/2,2).ne.0.and.(nwgeom+1)/2.ne.1)go to 570                   !/*pup4c2    337*/
      if(meshpl.gt.3)meshpl=0                                                   !/*pup4c2    338*/
      if(meshpl.eq.0.and.mcolor.lt.0)meshpl=1                                   !/*pup4c2    339*/
      if(meshpl.eq.0)hl='No Lines'                                              !/*pup4c2    340*/
      if(mod(meshpl,2).ne.0)hl='CellLine'                                       !/*pup4c2    341*/
      if(mod(meshpl/2,2).ne.0)hl=' WW MESH'                                     !/*pup4c2    342*/
      if(meshpl.ge.3)hl=' WW+Cell'                                              !/*pup4c2    343*/
      call gtx(real(xh+.56*ex(1)),real(-.44*ex(2)),hl)                          !/*pup4c2    344*/
      write(iuc,'(4hmesh,i2)')meshpl                                            !/*pup4c2    345*/
      go to 90                                                                  !/*pup4c2    346*/
!                                                                               !/*pup4c2    347*/
! >>>>>  postscript:  write plot postscript file.                               !/*pup4c2    348*/
  580 if(s1.gt.xh+.28*ex(1))go to 590                                           !/*pup4c2    349*/
      jgf=1                                                                     !/*pup4c2    350*/
      write(iuc,'(4hfile)')                                                     !/*pup4c2    351*/
      call gtx(xh,real(-.52*ex(2)),'PScript on')                                !/*pup4c2    352*/
      go to 90                                                                  !/*pup4c2    353*/
!                                                                               !/*pup4c2    354*/
! >>>>>  rotate:  rotate plot 90 degrees.                                       !/*pup4c2    355*/
  590 if(s1.gt.xh+.60*ex(1))go to 90                                            !/*pup4c2    356*/
      ir=1-ir                                                                   !/*pup4c2    357*/
      do 600 i=1,3                                                              !/*pup4c2    358*/
      ar=basis(i+3)                                                             !/*pup4c2    359*/
      basis(i+3)=basis(i)                                                       !/*pup4c2    360*/
  600 basis(i)=ar                                                               !/*pup4c2    361*/
      write(iuc,'(5hbasis,1p6e12.4)')(basis(i),i=1,6)                           !/*pup4c2    362*/
      if(ir.eq.0)write(hl,'(10hROTATE off)')                                    !/*pup4c2    363*/
      if(ir.ne.0)write(hl,'(10hROTATE  on)')                                    !/*pup4c2    364*/
      call gtx(real(xh+.32*ex(1)),real(-.52*ex(2)),hl)                          !/*pup4c2    365*/
      go to 90                                                                  !/*pup4c2    366*/
!                                                                               !/*pup4c2    367*/
! >>>>>  color:  toggle only, no resolution adjustment.                         !/*pup4c2    368*/
  630 if(s1.gt.xh+.60*ex(1))go to 650                                           !/*pupk       22*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+1))                              !/*pup4c2    370*/
      mcolor=-mcolor                                                            !/*pup4c2    371*/
      if(mcolor.gt.0)write(iuc,'(9hcolor  on)')                                 !/*pup4c2    372*/
      if(mcolor.lt.0)write(iuc,'(9hcolor off)')                                 !/*pup4c2    373*/
      if(mcolor.le.2.and.meshpl.eq.0)go to 570                                  !/*pup4c2    374*/
      if(mcolor.le.0)then                                                       !/*pupk       23*/
         call gtx(xh,real(-.60*ex(2)),'COLOR       off')                        !/*pupk       24*/
      else                                                                      !/*pupk       25*/
         call pltdat(1,jv,ip,nv,ie,ar,an,ha)                                    !/*pupk       26*/
         if(ie.ne.0)go to 90                                                    !/*pupk       27*/
         sclabl(5)=jv                                                           !/*pupk       28*/
         sclabl(6)=nv+1000*ip                                                   !/*pupk       29*/
         write(hl,'(5hCOLOR,a10)')ha(1:10)                                      !/*pupk       30*/
         call gtx(xh,real(-.60*ex(2)),hl(1:15))                                 !/*pupk       31*/
      endif                                                                     !/*pupk       32*/
      go to 90                                                                  !/*pup4c2    385*/
!                                                                               !/*pup4c2    386*/
! >>>>>  level:  plot at specified levels.                                      !/*pup4c2    387*/
  650 if(junf.eq.0)go to 90                                                     !/*pup4c2    388*/
      levplt=levplt+1                                                           !/*pup4c2    389*/
      if(levplt.ge.nlev)levplt=-1                                               !/*pup4c2    390*/
      if(levplt.lt.0)write(iuc,'(9hlevel off)')                                 !/*pup4c2    391*/
      if(levplt.ge.0)write(iuc,'(5hlevel,i4)')levplt                            !/*pup4c2    392*/
      if(levplt.lt.0)write(hl,'(7hLEVEL -)')                                    !/*pup4c2    393*/
      if(levplt.ge.0)write(hl,'(5hLEVEL,i2)')levplt                             !/*pup4c2    394*/
      call gtx(real(xh+.64*ex(1)),real(-.60*ex(2)),hl)                          !/*pup4c2    395*/
      go to 90                                                                  !/*pup4c2    396*/
!                                                                               !/*pup4c2    397*/
! >>>>>  xy/yz/zx:  change s and t axes to x-y, y-z, or z-x                     !/*pup4c2    398*/
  660 i=1.125+3.125*(s1-xh)/ex(1)                                               !/*pup4c2    399*/
      i=max(1,min(3,i))                                                         !/*pup4c2    400*/
      do 670 j=1,9                                                              !/*pup4c2    401*/
      basis(j)=0.                                                               !/*pup4c2    402*/
  670 if(j.eq.i.or.j.eq.mod(i,3)+4.or.j.eq.mod(i+1,3)+7)basis(j)=1.             !/*pup4c2    403*/
      write(iuc,'(5hbasis,1p6e12.4)')(basis(i),i=1,6)                           !/*pup4c2    404*/
      go to 900                                                                 !/*pup4c2    405*/
!                                                                               !/*pup4c2    406*/
! >>>>>  label:                                                                 !/*pup4c2    407*/
  680 call pltdat(1,jv,ip,nv,ie,ar,an,ha)                                       !/*pupk       33*/
      if(s1.lt.xh+.28*ex(1))write(iuc,'(5hlabel,0p2f5.2,a10)')                  !/*pup4c2    409*/
     & sclabl(1),sclabl(2),ha                                                   !/*pup4c2    410*/
      if(s1.lt.xh+.28*ex(1))go to 900                                           !/*pup4c2    411*/
      if(s1.gt.xh+.44*ex(1))go to 690                                           !/*pup4c2    412*/
      ar=sclabl(1)                                                              !/*pup4c2    413*/
      if(ar.eq.0.)sclabl(1)=c1                                                  !/*pup4c2    414*/
      if(ar.ne.0.)sclabl(1)=0.                                                  !/*pup4c2    415*/
      hl='off'                                                                  !/*pup4c2    416*/
      if(sclabl(1).ne.0.)hl='sur'                                               !/*pup4c2    417*/
      call gtx(real(xh+.32*ex(1)),real(-.76*ex(2)),hl)                          !/*pup4c2    418*/
      write(iuc,'(5hlabel,0p2f5.2,a10)')sclabl(1),sclabl(2),ha                  !/*pup4c2    419*/
      go to 90                                                                  !/*pup4c2    420*/
  690 ar=sclabl(2)                                                              !/*pup4c2    421*/
      if(ar.eq.0.)sclabl(2)=c2                                                  !/*pup4c2    422*/
      if(ar.ne.0..and.nint(sclabl(3)).eq.jv)sclabl(2)=0.                        !/*pup4c2    423*/
      hl='       off'                                                           !/*pup4c2    424*/
      if(sclabl(2).eq.0.)go to 700                                              !/*pup4c2    425*/
      sclabl(3)=jv                                                              !/*pup4c2    426*/
      sclabl(4)=nv+1000*ip                                                      !/*pup4c2    427*/
      hl=ha                                                                     !/*pup4c2    428*/
  700 call gtx(real(xh+.44*ex(1)),real(-.76*ex(2)),hl)                          !/*pup4c2    429*/
      write(iuc,'(5hlabel,0p2f5.2,a10)')sclabl(1),sclabl(2),ha                  !/*pup4c2    430*/
      go to 90                                                                  !/*pup4c2    431*/
!                                                                               !/*pup4c2    432*/
! >>>>>  mbody:                                                                 !/*pup4c2    433*/
  710 if(s1.gt.xh+.28*ex(1))go to 90                                            !/*pup4c2    434*/
      if(idne(lide+idne(lide+2)+4).lt.0)go to 90                                !/*pup4c2    435*/
      idne(lide+idne(lide+2)+4)=1-idne(lide+idne(lide+2)+4)                     !/*pup4c2    436*/
      if(idne(lide+idne(lide+2)+4).eq.0)write(iuc,'(9hmbody  on)')              !/*pup4c2    437*/
      if(idne(lide+idne(lide+2)+4).ne.0)write(iuc,'(9hmbody off)')              !/*pup4c2    438*/
      hl='MBODY  on'                                                            !/*pup4c2    439*/
      if(idne(lide+idne(lide+2)+4).ne.0)hl='MBODY off'                          !/*pup4c2    440*/
      call gtx(xh,real(-.84*ex(2)),hl)                                          !/*pup4c2    441*/
      go to 90                                                                  !/*pup4c2    442*/
!                                                                               !/*pup4c2    443*/
! >>>>>  enter plot commands from screen.                                       !/*pup4c2    444*/
  760 ln=-1                                                                     !/*pup4c2    445*/
  770 call pltscr                                                               !/*pup4c2    446*/
      if(ln.lt.0.and.klin.eq.' ')go to 90                                       !/*pup4c2    447*/
      write(iuc,'(a80)')klin                                                    !/*pup4c2    448*/
      if(klin.eq.' ')go to 900                                                  !/*pup4c2    449*/
      jchar=1                                                                   !/*pup4c2    450*/
      ln=0                                                                      !/*pup4c2    451*/
      inform=0                                                                  !/*pup4c2    452*/
  780 if(jdecod(ln).ne.0)go to 330                                              !/*pup4c2    453*/
      if(jchar.gt.80.or.klin(min(jchar,80):80).eq.' ')ln=1                      !/*pup4c2    454*/
      if(ln.eq.0)go to 780                                                      !/*pup4c2    455*/
      if(inform.eq.0)go to 900                                                  !/*pup4c2    456*/
      go to 770                                                                 !/*pup4c2    457*/
!                                                                               !/*pup4c2    458*/
! ************************ click in picture ****************************        !/*pup4c2    459*/
!                                                                               !/*pup4c2    460*/
!        get iap (cell) of clicked picture point from pltloc.                   !/*pup4c2    461*/
  800 if(lq.ge.10)go to 140                                                     !/*pup4c2    462*/
      kq=0                                                                      !/*pup4c2    463*/
      iq=iq+1                                                                   !/*pup4c2    464*/
      call pltloc(s1,t1)                                                        !/*pup4c2    465*/
!                                                                               !/*pup4c2    466*/
!        first click in picture.  mark location.                                !/*pup4c2    467*/
      xy(1,1)=s1-real(.011*ex(1))                                               !/*pup4c2    468*/
      xy(2,1)=s1+real(.011*ex(1))                                               !/*pup4c2    469*/
      xy(1,2)=t1                                                                !/*pup4c2    470*/
      xy(2,2)=t1                                                                !/*pup4c2    471*/
      call gpl(2,xy(1,1),xy(1,2))                                               !/*pup4c2    472*/
      xy(1,1)=s1                                                                !/*pup4c2    473*/
      xy(2,1)=s1                                                                !/*pup4c2    474*/
      xy(1,2)=t1-real(.011*ex(1))                                               !/*pup4c2    475*/
      xy(2,2)=t1+real(.011*ex(1))                                               !/*pup4c2    476*/
      call gpl(2,xy(1,1),xy(1,2))                                               !/*pup4c2    477*/
      go to 70                                                                  !/*pup4c2    478*/
!                                                                               !/*pup4c2    479*/
! **************************** exit ************************************        !/*pup4c2    480*/
!                                                                               !/*pup4c2    481*/
!        redraw (nq=1).                                                         !/*pup4c2    482*/
  900 nq=1                                                                      !/*pup4c2    483*/
      write(hl,'(29hProcessing ... Please Wait...)')                            !/*pup4c2    484*/
      go to 930                                                                 !/*pup4c2    485*/
!                                                                               !/*pup4c2    486*/
!        return to plot prompt (nq=0).                                          !/*pup4c2    487*/
  910 nq=0                                                                      !/*pup4c2    488*/
      write(hl,'(29hGo to command window ........)')                            !/*pup4c2    489*/
!                                                                               !/*pup4c2    490*/
  930 if(mcolor.gt.2)call gstxci(kcolor(ncolor+4))                              !/*pup4c2    491*/
      call gtx(xh,real(-ex(2)),hl)                                              !/*pup4c2    492*/
      if(mcolor.gt.2)call gstxci(kcolor(ncolor+1))                              !/*pup4c2    493*/
      call guwk(1,0)                                                            !/*pup4c2    494*/
#endif                                                                          /*!pupx       15*/
      return                                                                    !/*pup4c2    495*/
      end                                                                       !/*pup4c2    496*/
