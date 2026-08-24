!#ident grqst.F from deck grq4c2                                                !/*grq4c2      1*/
      subroutine grqst(nw,nd,ie,nc,hs)                                          !/*grq4c2      3*/
#ifdef GKSSIM                                                                   /*!grqx        1*/
!        perform gks function 'request string'.                                 !/*grq4c2      4*/
!        nw = workstation identifier                                            !/*grq4c2      5*/
!        nd = string device number                                              !/*grq4c2      6*/
!        ie = status                                                            !/*grq4c2      7*/
!        nc = number of characters returned                                     !/*grq4c2      8*/
!        hs = character string                                                  !/*grq4c2      9*/
      character hl*80,hs*80                                                     !/*grq4c2     10*/
      hl=' '                                                                    !/*grq4c2     11*/
#ifdef XLIB                                                                     /*!grq4c2     12*/
      call xgrqst(hl)                                                           !/*grq4c2     13*/
#endif                                                                          /*!cpp       362*/
      hs=hl                                                                     !/*grq4c2     14*/
#endif                                                                          /*!grqx        2*/
      return                                                                    !/*grq4c2     15*/
      end                                                                       !/*grq4c2     16*/
#ifdef GKSSIM                                                                   /*!gk         42*/
      subroutine gclks                                                          !/*gk         43*/
!        perform gks function 'close gks'.                                      !/*gk         44*/
      use gkssim_data                                                           !/*grxc        1*/
!                                                                               !/*gk         46*/
#ifdef CGS                                                                      /*!gk         50*/
      call gdone                                                                !/*gk         51*/
#endif                                                                          /*!cpp       364*/
#ifdef DISSPLA                                                                  /*!gk4b        8*/
#ifdef DISSCGS                                                                  /*!gk         53*/
!        level 1 to level 0                                                     !/*grqx        4*/
      call gdone                                                                !/*gk         54*/
#else                                                                           /*!grqx        5*/
!        level 1 to level 0                                                     !/*grqx        6*/
      call donepl                                                               !/*gk         56*/
#endif                                                                          /*!cpp       367*/
#endif                                                                          /*!cpp       365*/
      lvd=0                                                                     !/*gk         63*/
      return                                                                    !/*gk         64*/
      end                                                                       !/*gk         65*/
#endif                                                                          /*!cpp       363*/
#ifdef GKSSIM                                                                   /*!gk         67*/
      subroutine gopwk(nw,nc,nt)                                                !/*gk         68*/
!        perform gks function 'open workstation'.                               !/*gk         69*/
      use gkssim_data                                                           !/*grxc        2*/
!                                                                               !/*gk         71*/
#if defined(CGS) || defined(DISSCGS)                                            /*!gk         75*/
      call cgsdci(0,nw)                                                         !/*gk         76*/
#endif                                                                          /*!cpp       369*/
#ifdef XLIB                                                                     /*!gr4a       35*/
      if(nw.eq.1)call xgopwk(nc)                                                !/*gk4b        9*/
#endif                                                                          /*!cpp       370*/
      lvd=1                                                                     !/*gk         82*/
#if defined(XLIB) || defined(LAHEY) || defined(QWIN)                            /*!gk4c        1*/
      if(nw.eq.1)return                                                         !/*gk4b       11*/
!                                                                               !/*gk4b       12*/
!        initialize the postscript output file with unit number iup+50.         !/*gk4b       13*/
      munit=nc+50                                                               !/*gk4b       14*/
      write(munit,20)                                                           !/*gk4b       15*/
   20 format(2h%!,/29h%%DocumentFonts: Courier-Bold,                            !/*gk4b       16*/
     & /29h%%Title: MCNP Graphics Output,                                       !/*gk4b       17*/
     & /15h%%Creator: MCNP,/22h%%BoundingBox: 0 0 1 1)                          !/*gk4b       18*/
#endif                                                                          /*!cpp       371*/
      return                                                                    !/*gk         83*/
      end                                                                       !/*gk         84*/
#endif                                                                          /*!cpp       368*/
#ifdef GKSSIM                                                                   /*!gk         86*/
      subroutine gclwk(nw)                                                      !/*gk         87*/
!        perform gks function 'close workstation'.                              !/*gk         88*/
      use gkssim_data                                                           !/*grxc        3*/
!                                                                               !/*gk         90*/
#ifdef XLIB                                                                     /*!gr4a       37*/
      if(nw.eq.1)call xgclwk()                                                  !/*gk4b       19*/
#endif                                                                          /*!cpp       373*/
#if defined(XLIB) || defined(LAHEY)                                             /*!gk4b       20*/
      if(nw.eq.1)return                                                         !/*gk4b       21*/
!                                                                               !/*gk4b       22*/
!        write the postscript trailer.                                          !/*gk4b       23*/
      write(munit,20)npages                                                     !/*gk4b       24*/
   20 format(9h%%Trailer,/9h%%Pages: ,i3)                                       !/*gk4b       25*/
#endif                                                                          /*!cpp       374*/
      return                                                                    !/*gk         97*/
      end                                                                       !/*gk         98*/
#endif                                                                          /*!cpp       372*/
#ifdef GKSSIM                                                                   /*!gk        100*/
      subroutine gacwk(nw)                                                      !/*gk        101*/
!        perform gks function 'activate workstation'.                           !/*gk        102*/
#ifdef QWIN                                                                     /*!gk4c        2*/
      use dflib                                                                 !/*gk4c        3*/
      character h*20                                                            !/*gk4c        4*/
      logical*4 st                                                              !/*gk4c        5*/
      type (qwinfo) wi,wf                                                       !/*gk4c        6*/
      type (windowconfig) wc                                                    !/*gk4c        7*/
#endif                                                                          /*!cpp       376*/
      use gkssim_data                                                           !/*grxc        4*/
!                                                                               !/*gk        104*/
#if defined(CGS) || defined(DISSCGS)                                            /*!gk        105*/
      call cgsdci(1,nw)                                                         !/*gk        106*/
#endif                                                                          /*!cpp       377*/
#ifdef XLIB                                                                     /*!gr4a       42*/
      if(nw.eq.1)call xgacwk()                                                  !/*gk4b       26*/
#endif                                                                          /*!cpp       378*/
#ifdef QWIN                                                                     /*!gk4c        8*/
      if(nw.ne.1)go to 10                                                       !/*gk4c        9*/
!                                                                               !/*gk4c       10*/
!        move the execute window to the bottom of the frame.                    !/*gk4c       11*/
!        this assumes a font size of 8x16 (wf has units of pixels).             !/*gk4c       12*/
      k=getwsizeqq(qwin$framewindow,qwin$sizecurr,wf)                           !/*gk4c       13*/
      wi.type=qwin$set                                                          !/*gk4c       14*/
      wi.x=0                                                                    !/*gk4c       15*/
      wi.y=max(0,int(wf.h/16.)-32)                                              !/*gk4c       16*/
      wi.h=30                                                                   !/*gk4c       17*/
      wi.w=80                                                                   !/*gk4c       18*/
      k=setwsizeqq(0,wi)                                                        !/*gk4c       19*/
!                                                                               !/*gk4c       20*/
!        open the plot window and set its size (height=3/4 of width).           !/*gk4c       21*/
!        may not get this exact shape - use viewport to rectify.                !/*gk4c       22*/
      open(99,file='user')                                                      !/*gk4c       23*/
      k=setactiveqq(99)                                                         !/*gk4c       24*/
      wc.numypixels=max(480,min(int(wf.h*0.8),int(wf.w*3./4.)))                 !/*gk4c       25*/
      wc.numxpixels=int(4./3.*wc.numypixels)                                    !/*gk4c       26*/
      wc.numtextcols=-1                                                         !/*gk4c       27*/
      wc.numtextrows=-1                                                         !/*gk4c       28*/
      wc.numcolors=-1                                                           !/*gk4c       29*/
      wc.fontsize=-1                                                            !/*gk4c       30*/
      wc.title="MCNP Plot Window"C                                              !/*gk4c       31*/
      st=setwindowconfig(wc)                                                    !/*gk4c       32*/
      IF(.not.st)st=setwindowconfig(wc)                                         !/*gk4c       33*/
      st=getwindowconfig(wc)                                                    !/*gk4c       34*/
!                                                                               !/*gk4c       35*/
!        set font width based on 32 characters in 1/4 of window.                !/*gk4c       36*/
!        if font not found, use a 5x10 font.                                    !/*gk4c       37*/
      k=initializefonts()                                                       !/*gk4c       38*/
      write(h,'(a,i1)')'bfw',min(8,int(wc.numxpixels/4./32.))                   !/*gk4c       39*/
      k=setfont(h)                                                              !/*gk4c       40*/
      IF(k.lt.0)k=setfont('bw5')                                                !/*gk4c       41*/
!                                                                               !/*gk4c       42*/
!        once window is maximized, force it to stay that way.                   !/*gk4c       43*/
      wi.type=qwin$max                                                          !/*gk4c       44*/
      k=setwsizeqq(99,wi)                                                       !/*gk4c       45*/
   10 continue                                                                  !/*gk4c       46*/
#endif                                                                          /*!cpp       379*/
      jta(nw)=1                                                                 !/*gk        111*/
#if defined(XLIB) || defined(LAHEY) || defined(QWIN)                            /*!gk4c       47*/
!                                                                               !/*gk4b       28*/
!        for systems lacking metafiles set jta(2)=-1 for postscript.            !/*gk4b       29*/
      if(nw.eq.2)jta(nw)=-1                                                     !/*gk4b       30*/
#endif                                                                          /*!cpp       380*/
      return                                                                    !/*gk        112*/
      end                                                                       !/*gk        113*/
#endif                                                                          /*!cpp       375*/
#ifdef GKSSIM                                                                   /*!gk        115*/
      subroutine gdawk(nw)                                                      !/*gk        116*/
!        perform gks function 'deactivate workstation'.                         !/*gk        117*/
      use gkssim_data                                                           !/*grxc        5*/
!                                                                               !/*gk        119*/
#if defined(CGS) || defined(DISSCGS)                                            /*!gk        120*/
      call cgsdci(2,nw)                                                         !/*gk        121*/
#endif                                                                          /*!cpp       382*/
#ifdef XLIB                                                                     /*!gr4a       49*/
      if(nw.eq.1)call xgdawk()                                                  !/*gk4b       31*/
#endif                                                                          /*!cpp       383*/
#ifdef QWIN                                                                     /*!gk4c       48*/
      if(nw.eq.1)close(99)                                                      !/*gk4c       49*/
#endif                                                                          /*!cpp       384*/
      jta(nw)=0                                                                 !/*gk        126*/
      return                                                                    !/*gk        127*/
      end                                                                       !/*gk        128*/
#endif                                                                          /*!cpp       381*/
#ifdef GKSSIM                                                                   /*!gk        130*/
      subroutine gclrwk(nw,k)                                                   !/*gk        131*/
!        perform gks function 'clear workstation'.                              !/*gk        132*/
#ifdef QWIN                                                                     /*!gk4c       50*/
      use dflib                                                                 !/*gk4c       51*/
#endif                                                                          /*!cpp       386*/
      use gkssim_data                                                           !/*grxc        6*/
!                                                                               !/*gk        134*/
#ifdef CGS                                                                      /*!gk        137*/
      if(jta(3-nw).ne.0)call cgsdci(2,nw)                                       !/*gk        138*/
      call gpage                                                                !/*gk        139*/
      if(jta(3-nw).ne.0)call cgsdci(1,nw)                                       !/*gk        140*/
#endif                                                                          /*!cpp       387*/
#ifdef DISSCGS                                                                  /*!gk        141*/
!        level 1 or level 3 to level 1                                          !/*grqx       12*/
      if(lvd.eq.3)call endpl(0)                                                 !/*gk        142*/
      if(lvd.ne.1)go to 10                                                      !/*gk        143*/
      if(jta(3-nw).ne.0)call cgsdci(2,nw)                                       !/*gk        144*/
      call gpage                                                                !/*gk        145*/
      if(jta(3-nw).ne.0)call cgsdci(1,nw)                                       !/*gk        146*/
#endif                                                                          /*!cpp       388*/
#ifdef XLIB                                                                     /*!gr4a       53*/
      if(nw.eq.1)call xgclrw()                                                  !/*gk4b       32*/
#endif                                                                          /*!cpp       389*/
#ifdef QWIN                                                                     /*!gk4c       52*/
      if(nw.eq.1)j=setactiveqq(99)                                              !/*gk4c       53*/
      if(nw.eq.1)call clearscreen($gclearscreen)                                !/*gk4c       54*/
#endif                                                                          /*!cpp       390*/
   10 lvd=1                                                                     !/*gk        156*/
      return                                                                    !/*gk        157*/
      end                                                                       !/*gk        158*/
#endif                                                                          /*!cpp       385*/
#ifdef GKSSIM                                                                   /*!gk        160*/
      subroutine guwk(nw,k)                                                     !/*gk        161*/
!        perform gks function 'update workstation'.                             !/*gk        162*/
#ifdef QWIN                                                                     /*!gk4c       55*/
      use dflib                                                                 !/*gk4c       56*/
#endif                                                                          /*!cpp       392*/
      use gkssim_data                                                           !/*grxc        7*/
!                                                                               !/*gk        164*/
#if defined(CGS) || defined(DISSCGS)                                            /*!gk        167*/
      call gmakpc                                                               !/*gk        168*/
#endif                                                                          /*!cpp       393*/
#ifdef XLIB                                                                     /*!gr4a       57*/
      if(nw.eq.1.and.k.eq.0)call xguwk()                                        !/*gk4b       33*/
#endif                                                                          /*!cpp       394*/
#ifdef QWIN                                                                     /*!gk4c       57*/
      if(nw.eq.1.and.k.eq.0)j=focusqq(99)                                       !/*gk4c       58*/
#endif                                                                          /*!cpp       395*/
#if defined(XLIB) || defined(LAHEY) || defined(QWIN)                            /*!gk4c       59*/
      if(nw.eq.2)write(munit,'(16h stroke showpage)')                           !/*gk4b       35*/
#endif                                                                          /*!cpp       396*/
      return                                                                    !/*gk        173*/
      end                                                                       !/*gk        174*/
#endif                                                                          /*!cpp       391*/
#ifdef GKSSIM                                                                   /*!gk        176*/
      subroutine gpl(n,x,y)                                                     !/*gk        177*/
!        perform gks function 'polyline'.                                       !/*gk        178*/
#ifdef LAHEY                                                                    /*!gk4c       60*/
      use WINTERACTER                                                           !/*gk4c       61*/
#endif                                                                          /*!cpp       398*/
#ifdef QWIN                                                                     /*!gk4c       62*/
      use dflib                                                                 !/*gk4c       63*/
      type (wxycoord) xy                                                        !/*gk4c       64*/
#endif                                                                          /*!cpp       399*/
      use gkssim_data                                                           !/*grxc        8*/
      dimension cr(4,3),x(n),y(n),xw(20),yw(20)                                 !/*gk        180*/
      data cr/.006,.004,.006,.004,4*.003,.009,3*.003/                           !/*gk        181*/
!                                                                               !/*gk        182*/
!        set the line width.                                                    !/*gk        183*/
      if(jta(2).ne.0)write(munit,'(1x,f4.1,13h setlinewidth)')wsf               !/*gk4b       36*/
      if(jta(1)+jta(2).lt.0)go to 5                                             !/*gk4b       37*/
#ifdef CGS                                                                      /*!gk        184*/
      call gwidth(.1*(wsf-1.))                                                  !/*gk        185*/
#endif                                                                          /*!cpp       400*/
#ifdef DISSPLA                                                                  /*!gk        186*/
!        any level                                                              !/*grqx       15*/
      call thkcrv(.01*wsf)                                                      !/*gk        187*/
      call linear                                                               !/*gk        188*/
#endif                                                                          /*!cpp       401*/
#ifdef XLIB                                                                     /*!gr4a       59*/
      call xlnwid(wsf)                                                          !/*gr4a       60*/
#endif                                                                          /*!cpp       402*/
!                                                                               !/*gk        193*/
!        draw solid line.                                                       !/*gk        194*/
    5 if(ltype.gt.1)go to 20                                                    !/*gk4b       38*/
      if(jta(2).eq.0)go to 8                                                    !/*gk4b       39*/
!                                                                               !/*gk4b       40*/
!        draw a polyline using postscript commands.                             !/*gk4b       41*/
      do 6 i=1,n-1                                                              !/*gk4b       42*/
      if(mod(i,100).eq.0)write(munit,'(1x,6hstroke)')                           !/*gk4b       43*/
    6 write(munit,7)nint(nxnorm*(x(i)-xlf)/(xrt-xlf)),                          !/*gk4b       44*/
     & nint(nynorm*(y(i)-ybt)/(ytp-ybt)),                                       !/*gk4b       45*/
     & nint(nxnorm*(x(i+1)-xlf)/(xrt-xlf)),                                     !/*gk4b       46*/
     & nint(nynorm*(y(i+1)-ybt)/(ytp-ybt))                                      !/*gk4b       47*/
    7 format(1x,i6,1x,i6,8h moveto ,i6,1x,i6,7h lineto)                         !/*gk4b       48*/
    8 if(jta(1)+jta(2).lt.0)return                                              !/*gk4b       49*/
#ifdef CGS                                                                      /*!gk        200*/
      call gmova2(x(1),y(1))                                                    !/*gk        201*/
      do 10 i=2,n                                                               !/*gk        202*/
   10 call glina2(x(i),y(i))                                                    !/*gk        203*/
      call gwidth(0.)                                                           !/*gk        204*/
#endif                                                                          /*!cpp       403*/
#ifdef DISSPLA                                                                  /*!gk        205*/
!        level 3                                                                !/*grqx       16*/
      call curve(x,y,n,0)                                                       !/*gk        206*/
#endif                                                                          /*!cpp       404*/
#ifdef XLIB                                                                     /*!gr4a       61*/
      do 10 i=2,n                                                               !/*gr4a       62*/
   10 call xgpl(x(i-1),y(i-1),x(i),y(i))                                        !/*gr4a       63*/
#endif                                                                          /*!cpp       405*/
#ifdef QWIN                                                                     /*!gk4c       65*/
      call moveto_w(dble(x(1)),dble(y(1)),xy)                                   !/*gk4c       66*/
      do 10 i=2,n                                                               !/*gk4c       67*/
   10 k=lineto_w(dble(x(i)),dble(y(i)))                                         !/*gk4c       68*/
#endif                                                                          /*!cpp       406*/
#ifdef LAHEY                                                                    /*!gk4c       69*/
      call IGrMoveto(11.*(x(1)-xlf)/(xrt-xlf)*.75/chite(4),                     !/*gk4c       70*/
     & 8.5*(y(1)-ybt)/(ytp-ybt))                                                !/*gk4c       71*/
      do 10 i=2,n                                                               !/*gk4c       72*/
   10 call IGrLineTo(11.*(x(i)-xlf)/(xrt-xlf)*.75/chite(4),                     !/*gk4c       73*/
     & 8.5*(y(i)-ybt)/(ytp-ybt))                                                !/*gk4c       74*/
#endif                                                                          /*!cpp       407*/
      return                                                                    !/*gk        211*/
!                                                                               !/*gk        212*/
!        draw interrupted line.                                                 !/*gk        213*/
   20 xw(1)=x(1)                                                                !/*gk        214*/
      yw(1)=y(1)                                                                !/*gk        215*/
      m=1                                                                       !/*gk        216*/
      f=chite(4)/(chite(2)*chite(3))                                            !/*gk        217*/
      do 70 j=2,n                                                               !/*gk        218*/
      d=sqrt(((x(j)-x(j-1))/chite(1))**2+((y(j)-y(j-1))*f)**2)                  !/*gk        219*/
   30 c=cr(kls,ltype-1)-als                                                     !/*grxc        9*/
      bl=min(c,d)                                                               !/*grxc       10*/
      c=0.                                                                      !/*gk        221*/
      if(d.ne.0.)c=bl/d                                                         !/*gk        222*/
      xw(m+1)=xw(m)+c*(x(j)-xw(m))                                              !/*gk        223*/
      yw(m+1)=yw(m)+c*(y(j)-yw(m))                                              !/*gk        224*/
      m=m+1                                                                     !/*gk        225*/
      if(j.lt.n.and.bl.eq.d.and.m.lt.20)go to 60                                !/*gk        226*/
      if(mod(kls,2).eq.0)go to 50                                               !/*gk        227*/
      if(jta(2).eq.0)go to 38                                                   !/*gk4b       56*/
!                                                                               !/*gk4b       57*/
!        draw a polyline using postscript commands.                             !/*gk4b       58*/
      do 35 i=1,m-1                                                             !/*gk4b       59*/
   35 write(munit,7)nint(nxnorm*(xw(i)-xlf)/(xrt-xlf)),                         !/*gk4b       60*/
     & nint(nynorm*(yw(i)-ybt)/(ytp-ybt)),                                      !/*gk4b       61*/
     & nint(nxnorm*(xw(i+1)-xlf)/(xrt-xlf)),                                    !/*gk4b       62*/
     & nint(nynorm*(yw(i+1)-ybt)/(ytp-ybt))                                     !/*gk4b       63*/
      if(mod(j,100).eq.0)write(munit,'(1x,6hstroke)')                           !/*gk4b       64*/
   38 if(jta(1)+jta(2).lt.0)go to 50                                            !/*gk4b       65*/
#ifdef CGS                                                                      /*!gk        232*/
      call gmova2(xw(1),yw(1))                                                  !/*gk        233*/
      do 40 i=2,m                                                               !/*gk        234*/
   40 call glina2(xw(i),yw(i))                                                  !/*gk        235*/
#endif                                                                          /*!cpp       408*/
#ifdef DISSPLA                                                                  /*!gk        236*/
!        level 3                                                                !/*grqx       17*/
      call curve(xw,yw,m,0)                                                     !/*gk        237*/
#endif                                                                          /*!cpp       409*/
#ifdef XLIB                                                                     /*!gr4a       68*/
      do 40 i=2,m                                                               !/*gr4a       69*/
   40 call xgpl(xw(i-1),yw(i-1),xw(i),yw(i))                                    !/*gr4a       70*/
#endif                                                                          /*!cpp       410*/
#ifdef QWIN                                                                     /*!gk4c       75*/
      call moveto_w(dble(xw(1)),dble(yw(1)),xy)                                 !/*gk4c       76*/
      do 40 i=2,m                                                               !/*gk4c       77*/
   40 k=lineto_w(dble(xw(i)),dble(yw(i)))                                       !/*gk4c       78*/
#endif                                                                          /*!cpp       411*/
#ifdef LAHEY                                                                    /*!gk4c       79*/
      call IGrMoveTo(11.*(xw(1)-xlf)/(xrt-xlf)*.75/chite(4),                    !/*gk4c       80*/
     & 8.5*(yw(1)-ybt)/(ytp-ybt))                                               !/*gk4c       81*/
      do 40 i=2,m                                                               !/*gk4c       82*/
   40 call IGrLineTo(11.*(xw(i)-xlf)/(xrt-xlf)*.75/chite(4),                    !/*gk4c       83*/
     & 8.5*(yw(i)-ybt)/(ytp-ybt))                                               !/*gk4c       84*/
#endif                                                                          /*!cpp       412*/
   50 xw(1)=xw(m)                                                               !/*gk        242*/
      yw(1)=yw(m)                                                               !/*gk        243*/
      m=1                                                                       !/*gk        244*/
   60 if(bl.eq.d)go to 70                                                       !/*gk        245*/
      d=d-bl                                                                    !/*gk        246*/
      als=0.                                                                    !/*gk        247*/
      kls=mod(kls,4)+1                                                          !/*gk        248*/
      go to 30                                                                  !/*gk        249*/
   70 als=als+bl                                                                !/*gk        250*/
#ifdef CGS                                                                      /*!gk        251*/
      call gwidth(0.)                                                           !/*gk        252*/
#endif                                                                          /*!cpp       413*/
      return                                                                    !/*gk        253*/
      end                                                                       !/*gk        254*/
#endif                                                                          /*!cpp       397*/
#ifdef GKSSIM                                                                   /*!gk        256*/
      subroutine gtx(x,y,h)                                                     !/*gk        257*/
!        perform gks function 'text'.                                           !/*gk        258*/
#ifdef LAHEY                                                                    /*!gk4c       85*/
      use WINTERACTER                                                           !/*gk4c       86*/
#endif                                                                          /*!cpp       415*/
#ifdef QWIN                                                                     /*!gk4c       87*/
      use dflib                                                                 !/*gk4c       88*/
      type (wxycoord) xy                                                        !/*gk4c       89*/
#endif                                                                          /*!cpp       416*/
      use gkssim_data                                                           !/*grxc       11*/
      dimension ib(20)                                                          !/*gk        260*/
      character h*(*),hn*120                                                    !/*gk4b       72*/
      save ns                                                                   !/*gk4b       73*/
      data ns/0/                                                                !/*gk4b       74*/
!                                                                               !/*gk        262*/
      if(h.eq.' ')return                                                        !/*gk        263*/
      n=leng(h)                                                                 !/*gk        264*/
      call chrhol(h,ib,1)                                                       !/*gk        265*/
      if(jta(2).eq.0)go to 8                                                    !/*gk4b       75*/
!                                                                               !/*gk4b       76*/
!        postscript text commands - insert a backslash before ().               !/*gk4b       77*/
      j=0                                                                       !/*gk4b       78*/
      do 2 i=1,n                                                                !/*gk4b       79*/
      j=j+1                                                                     !/*gk4b       80*/
      if(index('()',h(i:i)).eq.0)go to 2                                        !/*gk4b       81*/
      hn(j:j)=char(92)                                                          !/*gk4b       82*/
      j=j+1                                                                     !/*gk4b       83*/
    2 hn(j:j)=h(i:i)                                                            !/*gk4b       84*/
      nx=nint(nxnorm*(x-xlf)/(xrt-xlf))                                         !/*gk4b       85*/
      ny=nint(nynorm*(y-ybt)/(ytp-ybt))                                         !/*gk4b       86*/
      np=nint(1.25*chite(5)*(abs(chup(1))*nxnorm/(xrt-xlf)+                     !/*gk4b       87*/
     & abs(chup(2))*nynorm/(ytp-ybt)))                                          !/*gk4b       88*/
      if(np.ne.ns)write(munit,4)np                                              !/*gk4b       89*/
    4 format(25h (Courier-Bold) findfont ,i6,18h scalefont setfont)             !/*gk4b       90*/
      if(abs(chup(1)).le.abs(chup(2)))write(munit,5)nx,ny,hn(1:j)               !/*gk4b       91*/
    5 format(1x,i6,1x,i6,9h moveto (,a,6h) show)                                !/*gk4b       92*/
      if(abs(chup(1)).gt.abs(chup(2)))write(munit,6)ny,-nx,hn(1:j)              !/*gk4b       93*/
    6 format(25h gsave newpath 90 rotate ,i6,1x,i6,7h moveto,/2h (,a,           !/*gk4b       94*/
     & 15h) show grestore)                                                      !/*gk4b       95*/
      ns=np                                                                     !/*gk4b       96*/
    8 if(jta(1)+jta(2).lt.0)return                                              !/*gk4b       97*/
#ifdef CGS                                                                      /*!gk        270*/
      call gtwdth(chite(5)*(-chup(1)+                                           !/*gk        271*/
     & chup(2)*chite(1)*chite(4)/(chite(2)*chite(3))))                          !/*gk        272*/
      call gmova2(x,y)                                                          !/*gk        273*/
      call gtext(ib,n)                                                          !/*gk        274*/
#endif                                                                          /*!cpp       417*/
#ifdef DISSPLA                                                                  /*!gk        275*/
!        level 3                                                                !/*grqx       19*/
      call height(11.*(chite(5)*(-chup(1)*chite(3)/chite(1)+                    !/*gk        276*/
     & chup(2)*chite(4)/chite(2))))                                             !/*gk        277*/
      call rlmess(ib,n,x,y)                                                     !/*gk        278*/
#endif                                                                          /*!cpp       418*/
#ifdef XLIB                                                                     /*!gr4a       75*/
      call xgtx(h,n,x,y)                                                        !/*gr4a       76*/
#endif                                                                          /*!cpp       419*/
#ifdef QWIN                                                                     /*!gk4c       90*/
!                                                                               !/*gk4c       91*/
!        top left of character string is the origin.                            !/*gk4c       92*/
!        font size is not set to chite(5) in gschup, but this works.            !/*gk4c       93*/
      x1=x                                                                      !/*gk4c       94*/
      y1=y                                                                      !/*gk4c       95*/
      if(abs(chup(1)).gt.abs(chup(2)))x1=x1-2.*chite(5)                         !/*gk4c       96*/
      IF(abs(chup(1)).le.abs(chup(2)))y1=y1+chite(5)                            !/*gk4c       97*/
      call moveto_w(dble(x1),dble(y1),xy)                                       !/*gk4c       98*/
      call outgtext(h(1:n))                                                     !/*gk4c       99*/
#endif                                                                          /*!cpp       420*/
#ifdef LAHEY                                                                    /*!gk4b       98*/
!        write vertical characters going down.                                  !/*grqx       20*/
      m=1                                                                       !/*gr4a       78*/
      if(abs(chup(1)).gt.abs(chup(2)))m=n                                       !/*gr4a       79*/
      ic=nint((x-xlf)/(xrt-xlf)*.75/chite(4)*10000)                             !/*gk4c      100*/
      if(abs(chup(1)).gt.abs(chup(2)))ic=max(0,ic-1)                            !/*gk4b      100*/
      il=max(0,nint((((ytp-y)/(ytp-ybt))-m+1)*10000))                           !/*gk4c      101*/
      do 10 i=1,m                                                               !/*gr4a       82*/
      call WindowOutString(ic,il,h(i:i+n-m))                                    !/*gk4c      102*/
   10 il=il+1                                                                   !/*gr4a       84*/
#endif                                                                          /*!cpp       421*/
      return                                                                    !/*gk        290*/
      end                                                                       !/*gk        291*/
#endif                                                                          /*!cpp       414*/
#ifdef GKSSIM                                                                   /*!gk        293*/
      subroutine gsln(l)                                                        !/*gk        294*/
!        perform gks function 'set linetype'.                                   !/*gk        295*/
      use gkssim_data                                                           !/*grxc       12*/
!                                                                               !/*gk        297*/
      ltype=max(1,min(4,l))                                                     !/*gk        298*/
      return                                                                    !/*gk        299*/
      end                                                                       !/*gk        300*/
#endif                                                                          /*!cpp       422*/
#ifdef GKSSIM                                                                   /*!gk        302*/
      subroutine gslwsc(s)                                                      !/*gk        303*/
!        perform gks function 'set linewidth scale factor'.                     !/*gk        304*/
      use gkssim_data                                                           !/*grxc       13*/
!                                                                               !/*gk        306*/
      wsf=s                                                                     !/*gk        307*/
      return                                                                    !/*gk        308*/
      end                                                                       !/*gk        309*/
#endif                                                                          /*!cpp       423*/
#ifdef GKSSIM                                                                   /*!gk        311*/
      subroutine gstxfp(i,j)                                                    !/*gk        312*/
!        perform gks function 'set text font and precision'.                    !/*gk        313*/
      use gkssim_data                                                           !/*grxc       14*/
!                                                                               !/*gk        315*/
      return                                                                    !/*gk        316*/
      end                                                                       !/*gk        317*/
#endif                                                                          /*!cpp       424*/
#ifdef GKSSIM                                                                   /*!gk        319*/
      subroutine gschxp(s)                                                      !/*gk        320*/
!        perform gks function 'set character expansion factor'.                 !/*gk        321*/
      use gkssim_data                                                           !/*grxc       15*/
!                                                                               !/*gk        323*/
      return                                                                    !/*gk        324*/
      end                                                                       !/*gk        325*/
#endif                                                                          /*!cpp       425*/
#ifdef GKSSIM                                                                   /*!gk        327*/
      subroutine gschh(t)                                                       !/*gk        328*/
!        perform gks function 'set character height'.                           !/*gk        329*/
      use gkssim_data                                                           !/*grxc       16*/
!                                                                               !/*gk        331*/
      chite(5)=t                                                                !/*gk4b      101*/
      if(jta(1)+jta(2).lt.0)return                                              !/*gk4b      102*/
#ifdef XLIB                                                                     /*!gr4a       85*/
      call xgschh(t)                                                            !/*gr4a       86*/
#endif                                                                          /*!cpp       427*/
      return                                                                    !/*gk        335*/
      end                                                                       !/*gk        336*/
#endif                                                                          /*!cpp       426*/
#ifdef GKSSIM                                                                   /*!gk        338*/
      subroutine gschup(x,y)                                                    !/*gk        339*/
!        perform gks function 'set character up vector'.                        !/*gk        340*/
#ifdef QWIN                                                                     /*!gk4c      103*/
      use dflib                                                                 !/*gk4c      104*/
#endif                                                                          /*!cpp       429*/
      use gkssim_data                                                           !/*grxc       17*/
!                                                                               !/*gk        342*/
      chup(1)=x                                                                 !/*gk        343*/
      chup(2)=y                                                                 !/*gk        344*/
      if(jta(1)+jta(2).lt.0)return                                              !/*gk4b      103*/
#ifdef CGS                                                                      /*!gk        345*/
      call gtbsln(y,-x)                                                         !/*gk        346*/
#endif                                                                          /*!cpp       430*/
#ifdef DISSPLA                                                                  /*!gk        347*/
!        any level                                                              !/*grqx       27*/
      call angle(atan2(-x,y)*57.2957795)                                        !/*gk        348*/
#endif                                                                          /*!cpp       431*/
#ifdef XLIB                                                                     /*!gr4a       87*/
      call xgschu(x,y)                                                          !/*gr4a       88*/
#endif                                                                          /*!cpp       432*/
#ifdef QWIN                                                                     /*!gk4c      105*/
!                                                                               !/*gk4c      106*/
!        tenths of degrees for counterclockwise rotation.                       !/*gk4c      107*/
      if(x.eq.0..and.y.gt.0.)k=0                                                !/*gk4c      108*/
      if(x.eq.0..and.y.lt.0.)k=1800                                             !/*gk4c      109*/
      if(x.lt.0..and.y.eq.0.)k=900                                              !/*gk4c      110*/
      if(x.gt.0..and.y.eq.0.)k=-900                                             !/*gk4c      111*/
      if(x.ne.0..and.y.ne.0.)k=nint(1800./3.14159265*atan2(x,y))-900            !/*gk4c      112*/
      call setgtextrotation(k)                                                  !/*gk4c      113*/
#endif                                                                          /*!cpp       433*/
      return                                                                    !/*gk        354*/
      end                                                                       !/*gk        355*/
#endif                                                                          /*!cpp       428*/
#ifdef GKSSIM                                                                   /*!gk        357*/
      subroutine gswn(nt,xm,xx,ym,yx)                                           !/*gk        358*/
!        perform gks function 'set window'.                                     !/*gk        359*/
#ifdef QWIN                                                                     /*!gk4c      114*/
      use dflib                                                                 !/*gk4c      115*/
#endif                                                                          /*!cpp       435*/
      use gkssim_data                                                           !/*grxc       18*/
!                                                                               !/*gk        361*/
      chite(1)=xx-xm                                                            !/*gk        362*/
      chite(2)=yx-ym                                                            !/*gk        363*/
      xlf=xm                                                                    !/*gk4b      104*/
      xrt=xx                                                                    !/*gk4b      105*/
      ybt=ym                                                                    !/*gk4b      106*/
      ytp=yx                                                                    !/*gk4b      107*/
      if(jta(1)+jta(2).lt.0)return                                              !/*gk4b      108*/
#ifdef CGS                                                                      /*!gk        366*/
      call gwind2(xm,xx,ym,yx)                                                  !/*gk        367*/
      call gtfnta                                                               !/*gk        368*/
#endif                                                                          /*!cpp       436*/
#ifdef DISSPLA                                                                  /*!gk        369*/
!        level 1 to level 2 to level 3                                          !/*grqx       29*/
      call physor(0.,0.)                                                        !/*gk        370*/
      call area2d(11.*chite(3),11.*chite(4))                                    !/*gk        371*/
      call xnonum                                                               !/*gk        372*/
      call ynonum                                                               !/*gk        373*/
      call xticks(0)                                                            !/*gk        374*/
      call yticks(0)                                                            !/*gk        375*/
      call graf(xm,xx-xm,xx,ym,yx-ym,yx)                                        !/*gk        376*/
#endif                                                                          /*!cpp       437*/
#ifdef XLIB                                                                     /*!gr4a       89*/
      call xgswn(xm,xx,ym,yx)                                                   !/*gr4a       90*/
#endif                                                                          /*!cpp       438*/
#ifdef QWIN                                                                     /*!gk4c      116*/
      k=setwindow(.true.,dble(xm),dble(ym),dble(xx),dble(yx))                   !/*gk4c      117*/
#endif                                                                          /*!cpp       439*/
      lvd=3                                                                     !/*gk        381*/
      return                                                                    !/*gk        382*/
      end                                                                       !/*gk        383*/
#endif                                                                          /*!cpp       434*/
#ifdef GKSSIM                                                                   /*!gk        385*/
      subroutine gsvp(nt,xm,xx,ym,yx)                                           !/*gk        386*/
!        perform gks function 'set viewport'.                                   !/*gk        387*/
#ifdef QWIN                                                                     /*!gk4c      118*/
      use dflib                                                                 !/*gk4c      119*/
      logical*4 st                                                              !/*gk4c      120*/
      type (windowconfig) wc                                                    !/*gk4c      121*/
#endif                                                                          /*!cpp       441*/
      use gkssim_data                                                           !/*grxc       19*/
!                                                                               !/*gk        389*/
      chite(3)=xx-xm                                                            !/*gk        390*/
      chite(4)=yx-ym                                                            !/*gk        391*/
#ifdef CGS                                                                      /*!gk        392*/
      call gview2(xm,xx,ym,yx)                                                  !/*gk        393*/
      call gtfnta                                                               !/*gk        394*/
#endif                                                                          /*!cpp       442*/
#ifdef QWIN                                                                     /*!gk4c      122*/
      st=getwindowconfig(wc)                                                    !/*gk4c      123*/
      call setviewport(int2(xm),int2(ym),min(int2(wc.numxpixels),               !/*gk4c      124*/
     & int2(wc.numypixels/yx)),int2(wc.numypixels))                             !/*gk4c      125*/
#endif                                                                          /*!cpp       443*/
      return                                                                    !/*gk        399*/
      end                                                                       !/*gk        400*/
#endif                                                                          /*!cpp       440*/
#ifdef GKSSIM                                                                   /*!gk        402*/
      subroutine gselnt(nt)                                                     !/*gk        403*/
!        perform gks function 'select normalization transformation'.            !/*gk        404*/
      use gkssim_data                                                           !/*grxc       20*/
!                                                                               !/*gk        406*/
      return                                                                    !/*gk        407*/
      end                                                                       !/*gk        408*/
#endif                                                                          /*!cpp       444*/
#ifdef GKSSIM                                                                   /*!gk        410*/
      subroutine gswkwn(nw,xm,xx,ym,yx)                                         !/*gk        411*/
!        perform gks function 'set workstation window'.                         !/*gk        412*/
      use gkssim_data                                                           !/*grxc       21*/
!                                                                               !/*gk        414*/
#ifdef CGS                                                                      /*!gk        415*/
      call gscrn2(xm,xx,ym,yx)                                                  !/*gk        416*/
#endif                                                                          /*!cpp       446*/
#ifdef DISSPLA                                                                  /*!gk        417*/
!        level 1                                                                !/*grqx       33*/
      call nobrdr                                                               !/*gk        418*/
      call ucchar                                                               !/*gk        419*/
      call hwscal(4hscre)                                                       !/*gk        420*/
      call page(11.*(xx-xm),11.*(yx-ym))                                        !/*gk        421*/
#endif                                                                          /*!cpp       447*/
#ifdef XLIB                                                                     /*!gk4b      109*/
      if(nw.eq.1)call xgswk(xm,xx,ym,yx)                                        !/*gk4b      110*/
#endif                                                                          /*!cpp       448*/
#if defined(XLIB) || defined(LAHEY) || defined(QWIN)                            /*!gk4c      126*/
      if(nw.eq.1)return                                                         !/*gk4b      112*/
      npages=npages+1                                                           !/*gk4b      113*/
!                                                                               !/*gk4b      114*/
!        setup the landscape postscript page - note psize(3)= usable            !/*gk4b      115*/
!        page width, psize(4)=usable page height, 300=print resolution          !/*gk4b      116*/
!        (dots/inch), 72=postscript conversion to inches, and the               !/*gk4b      117*/
!        origin is the lower left corner.                                       !/*gk4b      118*/
      nxnorm=nint(300.*psize(4))                                                !/*gk4b      119*/
      nynorm=nint(300.*psize(3))                                                !/*gk4b      120*/
      write(munit,20)-72.*(psize(3)+.5),72.*psize(4)*((xx-xm)/(yx-ym))*         !/*gk4b      121*/
     & .75/nxnorm,72.*psize(3)/nynorm,nint(nynorm/50.)                          !/*gk4b      122*/
   20 format(14h 90 rotate 36 ,f8.2,11h translate ,f9.5,1x,f9.5,                !/*gk4b      123*/
     &6h scale,/25h (Courier-Bold) findfont ,i6,18h scalefont setfont)          !/*gk4b      124*/
#endif                                                                          /*!cpp       449*/
      return                                                                    !/*gk        428*/
      end                                                                       !/*gk        429*/
#endif                                                                          /*!cpp       445*/
#ifdef GKSSIM                                                                   /*!gk        431*/
      subroutine ginlc(nw,j,i,x,y,ie,xm,xx,ym,yx,l,h)                           !/*gk        432*/
!        perform gks function 'initialize locator'.                             !/*gk        433*/
      use gkssim_data                                                           !/*grxc       22*/
      character h*(*)                                                           !/*gk        435*/
!                                                                               !/*gk        436*/
      return                                                                    !/*gk        439*/
      end                                                                       !/*gk        440*/
#endif                                                                          /*!cpp       450*/
#ifdef GKSSIM                                                                   /*!gk        442*/
      subroutine grqlc(nw,j,is,i,x,y)                                           !/*gk        443*/
#ifdef LAHEY                                                                    /*!gk4c      127*/
      use WINTERACTER                                                           !/*gk4c      128*/
#endif                                                                          /*!cpp       452*/
!        perform gks function 'request locator'.                                !/*gk        444*/
      use gkssim_data                                                           !/*grxc       23*/
#ifdef CGS                                                                      /*!grqx       36*/
      dimension p(3)                                                            !/*gk        446*/
!                                                                               !/*gk        447*/
      call gescpe(7hwcursor,p,3)                                                !/*gk        451*/
      x=p(1)                                                                    !/*gk        452*/
      y=p(2)                                                                    !/*gk        453*/
#endif                                                                          /*!cpp       453*/
#ifdef DISSPLA                                                                  /*!gk        454*/
!        level 3                                                                !/*grqx       37*/
      call hwdata(x,y)                                                          !/*gk        455*/
#endif                                                                          /*!cpp       454*/
#ifdef XLIB                                                                     /*!gr4a       96*/
      call xgrqlc(j,x,y)                                                        !/*gr4a       97*/
#endif                                                                          /*!cpp       455*/
#ifdef LAHEY                                                                    /*!gk4c      129*/
      type (win_message) ::mesg                                                 !/*gk4c      130*/
      logical click                                                             !/*gk4c      131*/
      do 10 i=1,10                                                              !/*gk4c      132*/
   10 call WMessageEnable(i,0)                                                  !/*gk4c      133*/
      call WindowOutString(100,9500,'Click for Cursor Location')                !/*gk4c      134*/
      call WindowOutString(100,9700,'Click on Title Bar to Redraw')             !/*gk4c      135*/
      call WindowOutString(100,9900,'Drag on Title Bar to Move Window')         !/*gk4c      136*/
      call WMessageEnable(MouseButDown,1)                                       !/*gk4c      137*/
      call WMessageEnable(Expose,1)                                             !/*gk4c      138*/
      click=.false.                                                             !/*gk4c      139*/
      dowhile (.not.click)                                                      !/*gk4c      140*/
       call WmessagePeek(itype,mesg)                                            !/*gk4c      141*/
       select case(itype)                                                       !/*gk4c      142*/
        case(Expose)                                                            !/*gk4c      143*/
         call gxon(1)                                                           !/*gk4c      144*/
         call tekdvr                                                            !/*gk4c      145*/
         call WindowOutString(100,9500,'Click for Cursor Location')             !/*gk4c      146*/
         call WindowOutString(100,9700,'Click on Title Bar to Redraw')          !/*gk4c      147*/
       call WindowOutString(100,9900,'Drag on Title Bar to Move Window')        !/*gk4c      148*/
        case(mousebutdown)                                                      !/*gk4c      149*/
         click=.true.                                                           !/*gk4c      150*/
       end select                                                               !/*gk4c      151*/
      enddo                                                                     !/*gk4c      152*/
      call WMessageEnable(MouseButDown,0)                                       !/*gk4c      153*/
      call WMessageEnable(Expose,0)                                             !/*gk4c      154*/
      call gxon(1)                                                              !/*gk4c      155*/
      call tekdvr                                                               !/*gk4c      156*/
      call WindowClearArea(0,9700,2000,9990)                                    !/*gk4c      157*/
      x=mesg%gx * (xrt-xlf)/11.0/0.75*chite(4)  + xlf                           !/*gk4c      158*/
      y=mesg%gy * (ytp-ybt)/8.5   + ybt                                         !/*gk4c      159*/
!         draw cross hairs.                                                     !/*gk4c      160*/
      do 30 i=-1,1,2                                                            !/*gk4c      161*/
      call IGrColourN(40)                                                       !/*gk4c      162*/
      call IGrMoveTo(mesg%gx,mesg%gy)                                           !/*gk4c      163*/
      call IGrLineTo(mesg%gx+i*0.1,mesg%gy)                                     !/*gk4c      164*/
      call IGrMoveTo(mesg%gx,mesg%gy)                                           !/*gk4c      165*/
   30 call IGrLineTo(mesg%gx,mesg%gy+i*0.1)                                     !/*gk4c      166*/
#endif                                                                          /*!cpp       456*/
      return                                                                    !/*gk        460*/
      end                                                                       !/*gk        461*/
#endif                                                                          /*!cpp       451*/
#ifdef GKSSIM                                                                   /*!gr4a       98*/
      subroutine gsfais(is)                                                     !/*gr4a       99*/
!         perform gks function 'set fill area interior style'.                  !/*gr4a      100*/
      use gkssim_data                                                           !/*grxc       24*/
!                                                                               !/*gr4a      102*/
      return                                                                    !/*gr4a      103*/
      end                                                                       !/*gr4a      104*/
#endif                                                                          /*!cpp       457*/
#ifdef GKSSIM                                                                   /*!gr4a      106*/
      subroutine gfa(n,x,y)                                                     !/*gr4a      107*/
!         perform gks function 'fill area'.                                     !/*gr4a      108*/
#ifdef LAHEY                                                                    /*!gk4c      167*/
      use WINTERACTER                                                           !/*gk4c      168*/
#endif                                                                          /*!cpp       459*/
#ifdef QWIN                                                                     /*!gk4c      169*/
      use dflib                                                                 !/*gk4c      170*/
#endif                                                                          /*!cpp       460*/
      use gkssim_data                                                           !/*grxc       25*/
!                                                                               !/*gr4a      110*/
      dimension nx(10),ny(10),x(n),y(n)                                         !/*grqx       40*/
#ifdef LAHEY                                                                    /*!grqx       41*/
      dimension xn(10),yn(10)                                                   !/*grqx       42*/
#endif                                                                          /*!grqx       43*/
      if(jta(2).eq.0)go to 5                                                    !/*gk4b      126*/
!                                                                               !/*gk4b      127*/
!        fill rectangles using postscript commands.                             !/*gk4b      128*/
      do 2 i=1,n                                                                !/*gk4b      129*/
      nx(i)=nint(nxnorm*(x(i)-xlf)/(xrt-xlf))                                   !/*gk4b      130*/
    2 ny(i)=nint(nynorm*(y(i)-ybt)/(ytp-ybt))                                   !/*gk4b      131*/
      write(munit,4)nx(1),ny(1),nx(2)-nx(1),ny(3)-ny(2),nx(4)-nx(3)             !/*gk4c      171*/
    4 format(15h gsave newpath ,i6,1x,i6,8h moveto ,i6,13h 0 rlineto 0 ,        !/*gk4b      133*/
     & i6,9h rlineto ,/1x,i6,34h 0 rlineto closepath fill grestore)             !/*gk4b      134*/
    5 if(jta(1)+jta(2).lt.0)return                                              !/*gk4b      135*/
#ifdef XLIB                                                                     /*!gr4a      112*/
      call xgfa(x(1),y(1),x(3),y(3))                                            !/*gr4a      113*/
#endif                                                                          /*!cpp       461*/
#ifdef QWIN                                                                     /*!gk4c      172*/
      k=rectangle_w($gfillinterior,dble(x(1)),dble(y(1)),                       !/*gk4c      173*/
     & dble(x(3)),dble(y(3)))                                                   !/*gk4c      174*/
#endif                                                                          /*!cpp       462*/
#ifdef LAHEY                                                                    /*!gk4c      175*/
      do 10 i=1,n                                                               !/*gr4a      115*/
      xn(i)=11.*(x(i)-xlf)/(xrt-xlf)*.75/chite(4)                               !/*gk4b      136*/
   10 yn(i)=8.5*(y(i)-ybt)/(ytp-ybt)                                            !/*gk4b      137*/
      call IGrFillPattern(4,2,3)                                                !/*gk4c      176*/
      call IGrPolygonComplex(xn,yn,n)                                           !/*gk4c      177*/
#endif                                                                          /*!cpp       463*/
      return                                                                    !/*gr4a      119*/
      end                                                                       !/*gr4a      120*/
#endif                                                                          /*!cpp       458*/
#ifdef GKSSIM                                                                   /*!gr4a      122*/
      subroutine gsfaci(ic)                                                     !/*gr4a      123*/
!         perform gks function 'set fill area color index'.                     !/*gr4a      124*/
#ifdef LAHEY                                                                    /*!gk4c      178*/
      use WINTERACTER                                                           !/*gk4c      179*/
#endif                                                                          /*!cpp       465*/
#ifdef QWIN                                                                     /*!gk4c      180*/
      use dflib                                                                 !/*gk4c      181*/
#endif                                                                          /*!cpp       466*/
      use gkssim_data                                                           !/*grxc       26*/
!                                                                               !/*gr4a      126*/
      if(jta(2).ne.0.and.ic.ne.icol)write(munit,10)                             !/*gk4b      139*/
     & rgb(3*(ic-1)+1),rgb(3*(ic-1)+2),rgb(3*(ic-1)+3)                          !/*gk4b      140*/
   10 format(1x,f4.2,1x,f4.2,1x,f4.2,12h setrgbcolor)                           !/*gk4b      141*/
      icol=ic                                                                   !/*gk4b      142*/
      if(jta(1)+jta(2).lt.0)return                                              !/*gk4b      143*/
#ifdef XLIB                                                                     /*!gr4a      127*/
!        use the polyline call.                                                 !/*grqx       45*/
      call xgsplc(ic)                                                           !/*gr4a      128*/
#endif                                                                          /*!cpp       467*/
#ifdef QWIN                                                                     /*!gk4c      182*/
      k=setcolor(int2(ic))                                                      !/*gk4c      183*/
#endif                                                                          /*!cpp       468*/
#ifdef LAHEY                                                                    /*!gr4a      129*/
      call IGrColourN(16*(ic-1))                                                !/*gk4c      184*/
#endif                                                                          /*!cpp       469*/
      return                                                                    !/*gr4a      131*/
      end                                                                       !/*gr4a      132*/
#endif                                                                          /*!cpp       464*/
#ifdef GKSSIM                                                                   /*!gr4a      134*/
      subroutine gsplci(ic)                                                     !/*gr4a      135*/
!         perform gks function 'set polyline color index'.                      !/*gr4a      136*/
#ifdef LAHEY                                                                    /*!gk4c      185*/
      use WINTERACTER                                                           !/*gk4c      186*/
#endif                                                                          /*!cpp       471*/
#ifdef QWIN                                                                     /*!gk4c      187*/
      use dflib                                                                 !/*gk4c      188*/
#endif                                                                          /*!cpp       472*/
      use gkssim_data                                                           !/*grxc       27*/
!                                                                               !/*gr4a      138*/
      if(jta(2).ne.0.and.ic.ne.icol)write(munit,10)                             !/*gk4b      144*/
     & rgb(3*(ic-1)+1),rgb(3*(ic-1)+2),rgb(3*(ic-1)+3)                          !/*gk4b      145*/
   10 format(1x,6hstroke,1x,f4.2,1x,f4.2,1x,f4.2,12h setrgbcolor)               !/*gk4b      146*/
      icol=ic                                                                   !/*gk4b      147*/
      if(jta(1)+jta(2).lt.0)return                                              !/*gk4b      148*/
#ifdef XLIB                                                                     /*!gr4a      139*/
      call xgsplc(ic)                                                           !/*gr4a      140*/
#endif                                                                          /*!cpp       473*/
#ifdef QWIN                                                                     /*!gk4c      189*/
      k=setcolor(int2(ic))                                                      !/*gk4c      190*/
#endif                                                                          /*!cpp       474*/
#ifdef LAHEY                                                                    /*!gr4a      141*/
      call IGrColourN(16*(ic-1))                                                !/*gk4c      191*/
#endif                                                                          /*!cpp       475*/
      return                                                                    !/*gr4a      143*/
      end                                                                       !/*gr4a      144*/
#endif                                                                          /*!cpp       470*/
#ifdef GKSSIM                                                                   /*!gr4a      146*/
      subroutine gstxci(ic)                                                     !/*gr4a      147*/
!         perform gks function 'set text color index'.                          !/*gr4a      148*/
#ifdef LAHEY                                                                    /*!gk4c      192*/
      use WINTERACTER                                                           !/*gk4c      193*/
      type (win_font) :: MCNPfont                                               !/*gk4c      194*/
#endif                                                                          /*!cpp       477*/
#ifdef QWIN                                                                     /*!gk4c      195*/
      use dflib                                                                 !/*gk4c      196*/
#endif                                                                          /*!cpp       478*/
      use gkssim_data                                                           !/*grxc       28*/
!                                                                               !/*gr4a      150*/
      if(jta(2).ne.0.and.ic.ne.icol)write(munit,10)                             !/*gk4b      149*/
     & rgb(3*(ic-1)+1),rgb(3*(ic-1)+2),rgb(3*(ic-1)+3)                          !/*gk4b      150*/
   10 format(1x,f4.2,1x,f4.2,1x,f4.2,12h setrgbcolor)                           !/*gk4b      151*/
      icol=ic                                                                   !/*gk4b      152*/
      if(jta(1)+jta(2).lt.0)return                                              !/*gk4b      153*/
#ifdef XLIB                                                                     /*!gr4a      151*/
!        use the polyline call.                                                 !/*grqx       48*/
      call xgsplc(ic)                                                           !/*gr4a      152*/
#endif                                                                          /*!cpp       479*/
#ifdef QWIN                                                                     /*!gk4c      197*/
      k=setcolor(int2(ic))                                                      !/*gk4c      198*/
#endif                                                                          /*!cpp       480*/
#ifdef LAHEY                                                                    /*!gk4c      199*/
      MCNPfont%ifontnum = 0                                                     !/*gk4c      200*/
      MCNPfont%ifcol = (ic-1)/2                                                 !/*gk4c      201*/
      MCNPfont%ibcol = -1                                                       !/*gk4c      202*/
      MCNPfont%iwidth = 100                                                     !/*gk4c      203*/
      MCNPfont%iheight = 200                                                    !/*gk4c      204*/
      MCNPfont%ibold = 1                                                        !/*gk4c      205*/
      call WindowFont(MCNPfont)                                                 !/*gk4c      206*/
#endif                                                                          /*!cpp       481*/
      return                                                                    !/*gr4a      155*/
      end                                                                       !/*gr4a      156*/
#endif                                                                          /*!cpp       476*/
#ifdef GKSSIM                                                                   /*!gr4a      158*/
      subroutine gqcf(nt,ie,nc,ic,ni)                                           !/*gr4a      159*/
!         perform gks function 'inquire color facilities'.                      !/*gr4a      160*/
#ifdef LAHEY                                                                    /*!gk4c      207*/
      use WINTERACTER                                                           !/*gk4c      208*/
      type (win_style) :: MCNPwin                                               !/*gk4c      209*/
      type (win_font) :: MCNPfont                                               !/*gk4c      210*/
#endif                                                                          /*!cpp       483*/
#ifdef QWIN                                                                     /*!gk4c      211*/
      use dflib                                                                 !/*gk4c      212*/
      type (windowconfig) wc                                                    !/*gk4c      213*/
#endif                                                                          /*!cpp       484*/
      use gkssim_data                                                           !/*grxc       29*/
#ifdef LAHEY                                                                    /*!gr4a      162*/
      dimension ia(9),ra(7)                                                     !/*gr4a      163*/
      character hl*40                                                           !/*gr4a      164*/
#endif                                                                          /*!cpp       485*/
!                                                                               !/*gr4a      165*/
      ic=0                                                                      !/*gr4a      166*/
#ifdef XLIB                                                                     /*!gr4a      167*/
      call xgqcf(ic,ni)                                                         !/*gr4a      168*/
#endif                                                                          /*!cpp       486*/
#ifdef QWIN                                                                     /*!gk4c      214*/
      k=getwindowconfig(wc)                                                     !/*gk4c      215*/
      ni=wc.numcolors                                                           !/*gk4c      216*/
      if(ni.gt.2)ic=1                                                           !/*gk4c      217*/
#endif                                                                          /*!cpp       487*/
#ifdef LAHEY                                                                    /*!gk4c      218*/
      call WInitialise(' ')                                                     !/*gk4c      219*/
      MCNPwin%flags = MinButton + MaxButton                                     !/*gk4c      220*/
      MCNPwin%x = 0                                                             !/*gk4c      221*/
      MCNPwin%y = 0                                                             !/*gk4c      222*/
      MCNPwin%width = 0                                                         !/*gk4c      223*/
      MCNPwin%height = 0                                                        !/*gk4c      224*/
      MCNPwin%menuid = 0                                                        !/*gk4c      225*/
      MCNPwin%title = "MCNP Plot Window"                                        !/*gk4c      226*/
      call WindowOpen(MCNPwin)                                                  !/*gk4c      227*/
      call IGrArea(0.0,0.0,1.0,1.0)                                             !/*gk4c      228*/
      call IGrUnits(0.0,0.0,11.0,8.5)                                           !/*gk4c      229*/
      ni=InfoGrScreen(30)                                                       !/*gk4c      230*/
      MCNPfont%ifontnum = 4                                                     !/*gk4c      231*/
      MCNPfont%iwidth = 100                                                     !/*gk4c      232*/
      MCNPfont%iheight = 200                                                    !/*gk4c      233*/
      MCNPfont%ibold = 1                                                        !/*gk4c      234*/
      call WindowFont(MCNPfont)                                                 !/*gk4c      235*/
      nctext=10000/WInfoFont(9)                                                 !/*gk4c      236*/
      nltext=10000/WInfoFont(10)                                                !/*gk4c      237*/
      call WindowClose                                                          !/*gk4c      238*/
      if(ni.gt.2)ic=1                                                           !/*gk4c      239*/
#endif                                                                          /*!cpp       488*/
      return                                                                    !/*gr4a      175*/
      end                                                                       !/*gr4a      176*/
#endif                                                                          /*!cpp       482*/
#ifdef GKSSIM                                                                   /*!gr4a      178*/
      subroutine gscr(nw,ic,r,g,b)                                              !/*gr4a      179*/
!         perform gks function 'set color representation'.                      !/*gr4a      180*/
      use gkssim_data                                                           !/*grxc       30*/
!                                                                               !/*gr4a      182*/
      j=3*(ic-1)                                                                !/*gk4c      240*/
      rgb(j+1)=r                                                                !/*gk4c      241*/
      rgb(j+2)=g                                                                !/*gk4c      242*/
      rgb(j+3)=b                                                                !/*gk4c      243*/
#ifdef XLIB                                                                     /*!grqk        1*/
      call xgscr(ic,r,g,b)                                                      !/*grqk        2*/
#endif                                                                          /*!grqk        3*/
      return                                                                    !/*gr4a      183*/
      end                                                                       !/*gr4a      184*/
#endif                                                                          /*!cpp       489*/
#ifdef GKSSIM                                                                   /*!gr4a      186*/
      subroutine gsds(nw,id,ir)                                                 !/*gr4a      187*/
!         perform gks function 'set deferral state'.                            !/*gr4a      188*/
      use gkssim_data                                                           !/*grxc       31*/
!                                                                               !/*gr4a      190*/
      return                                                                    !/*gr4a      191*/
      end                                                                       !/*gr4a      192*/
#endif                                                                          /*!cpp       490*/
