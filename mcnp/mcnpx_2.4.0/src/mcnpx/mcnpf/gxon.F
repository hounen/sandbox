!#ident gxon.F from deck gr                                                     !/*gr          1*/
      subroutine gxon(nw)                                                       !/*gr          3*/
#if defined(PLOT) || defined(MCPLOT)                                            /*!grx         1*/
#ifdef LAHEY                                                                    /*!gr4c        1*/
      use WINTERACTER                                                           !/*gr4c        2*/
#endif                                                                          /*!cpp       333*/
!        turn on workstation nw.  nw=1 for terminal, 2 for metafile.            !/*gr          4*/
      use global_data                                                           !/*grx         2*/
!                                                                               !/*grx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*grx         4*/
      implicit integer(kindi) (i-n)                                             !/*grx         5*/
!                                                                               !/*grx         6*/
#ifdef LAHEY                                                                    /*!gr4c        3*/
      type (win_style) :: MCNPwin                                               !/*gr4c        4*/
#endif                                                                          /*!cpp       334*/
      dimension kn(2)                                                           !/*grxk        1*/
      character hf*12                                                           !/*gr4b        1*/
!                                                                               !/*gr4a        3*/
!        rgb triplets for each color - see pbldat for colors.                   !/*gr4a        4*/
!        the seven line colors beyond ncolor are:  black, blue,                 !/*gr4c        7*/
      real :: rg(3,ncolor+7) = reshape ( (/ (0.,k=1,3*ncolor),                  !/*grxk        2*/
     & 0.00,0.00,0.00, 0.00,0.00,1.00, 1.00,0.00,0.00, 0.00,1.00,0.00,          !/*grxk        3*/
     & 0.75,0.00,1.00, 1.00,1.00,0.00, 1.00,1.00,1.00 /),                       !/*grxk        4*/
     & (/ 3,ncolor+7 /) )                                                       !/*grxk        5*/
!                                                                               !/*gr4a       10*/
!        system dependent color indices - negative to skip rgb values.          !/*gr4a       11*/
#if !defined(XLIB) && !defined(LAHEY) && !defined(QWIN)                         /*!gr4c       10*/
      data kc/2,3,4,5,6,7,8,9,10,11,-1,3,12,4,2,5,0/                            !/*gr4c       11*/
#endif                                                                          /*!cpp       335*/
#ifdef XLIB                                                                     /*!gr4c       12*/
      integer(kindi) :: kc(ncolor+7) = (/ (k, k=1,ncolor+7) /)                  !/*grxk        6*/
#endif                                                                          /*!cpp       336*/
#if defined(QWIN) || defined(LAHEY)                                             /*!gr4c       14*/
      data kc/5,1,2,14,7,11,6,8,9,10,15,1,4,2,5,14,0/                           !/*gr4c       15*/
#endif                                                                          /*!cpp       337*/
!                                                                               !/*gr          7*/
      kn(1)=jtty                                                                !/*gr          8*/
      kn(2)=iup                                                                 !/*gr          9*/
      if(jgxa(nw).ne.0)go to 20                                                 !/*gr         10*/
      if(jgxo(nw).ne.0)go to 10                                                 !/*gr         11*/
!                                                                               !/*gr         12*/
!        open gks if it has not already been done.                              !/*gr         13*/
      if(jgxo(3-nw).eq.0)call gopks(jtty,-1)                                    !/*gr         14*/
      call gselnt(1)                                                            !/*gr         15*/
      call gstxfp(1,1)                                                          !/*gr         16*/
!                                                                               !/*gr         17*/
!        open workstation nw.                                                   !/*gr         18*/
      if(nw.eq.1)jt=iterm                                                       !/*gr         19*/
      if(nw.eq.2)jt=1                                                           !/*gr         20*/
#if !defined(XLIB) && !defined(LAHEY) && !defined(QWIN)                         /*!gr4c       16*/
      if(nw.eq.2)call unique(plotm,jtty)                                        !/*gr         21*/
#else                                                                           /*!grx         7*/
      hf=plotm(1:leng(plotm))//'.ps'                                            !/*gr4b        6*/
      if(nw.eq.2)call unique(hf,jtty)                                           !/*gr4b        7*/
      if(nw.eq.2)open(iup+50,file=hf,status='unknown')                          !/*gr4b        8*/
#endif                                                                          /*!cpp       339*/
!        these next three are needed for gks at nmfecc.                         !/*gr         22*/
!      if(nw.eq.1)call gs4010ns(jt)                                             !/*gr         23*/
!      if(nw.eq.1)call gscnid(1,' ')                                            !/*gr         24*/
!      if(nw.eq.1)kn(nw)=1                                                      !/*gr         25*/
      call gopwk(nw,kn(nw),jt)                                                  !/*gr         26*/
      if(kn(nw).lt.0)return                                                     !/*gr4a       18*/
      jgxo(nw)=1                                                                !/*gr         27*/
!                                                                               !/*gr4a       19*/
!        check for a color display and initialize color indices.                !/*gr4a       20*/
!        if terminal and metafile output devices are used, the terminal         !/*gr4b        9*/
!        color status is applied to metafiles.  if only a metafile              !/*gr4b       10*/
!        device is used, the default color status is on.                        !/*gr4b       11*/
      if(nw.eq.2.and.jgxo(1).ne.0)go to 10                                      !/*gr4b       12*/
      ic=1                                                                      !/*gr4b       13*/
      ni=ncolor                                                                 !/*gr4b       14*/
      if(nw.eq.1)call gqcf(jt,ie,nc,ic,ni)                                      !/*gr4b       15*/
      if(ic.eq.0.or.ni.le.2)go to 10                                            !/*gr4a       22*/
      mcolor=sign(min(ni,ncolor),mcolor)                                        !/*gr4a       23*/
!                                                                               !/*grxk        7*/
!        set the color rgb values.                                              !/*grxk        8*/
      call rgbval(ncolor,430.,650.,rg)                                          !/*grxk        9*/
      do 5 i=1,ncolor+7                                                         !/*gr4c       18*/
#if defined(LAHEY) || defined(QWIN)                                             /*!gr4c       19*/
      if(i.le.ncolor)kcolor(i)=i                                                !/*gr4c       20*/
      if(i.le.7)kcolor(ncolor+i)=i                                              !/*gr4c       21*/
      if(ni.lt.ncolor+7)go to 5                                                 !/*gr4c       22*/
#endif                                                                          /*!cpp       340*/
      kcolor(i)=abs(kc(i))                                                      !/*gr4a       29*/
      if(kc(i).gt.0)call gscr(nw,kcolor(i),rg(1,i),rg(2,i),rg(3,i))             !/*gr4a       30*/
    5 continue                                                                  !/*gr4a       31*/
!                                                                               !/*gr         28*/
!        activate workstation nw.                                               !/*gr         29*/
   10 call gacwk(nw)                                                            !/*gr         30*/
      call gsds(nw,0,0)                                                         !/*gr4a       32*/
      jgxa(nw)=1                                                                !/*gr         31*/
      if(nw.eq.1)call gclrwk(1,1)                                               !/*gr         32*/
!                                                                               !/*gr         33*/
!        set the workstation window and the viewport.                           !/*gr         34*/
   20 if(nw.ne.1.or.iterm.ne.4)call gswkwn(nw,0.,1.,0.,.75+jvp*.25)             !/*gr         35*/
      call gsvp(1,0.,1.,0.,.75+jvp*.25)                                         !/*gr         36*/
      wnvp(3)=1.                                                                !/*gr         37*/
      wnvp(4)=.75+jvp*.25                                                       !/*gr         38*/
#ifdef LAHEY                                                                    /*!gr4c       23*/
      if(nw.eq.1)then                                                           !/*gr4c       24*/
         call WInitialise(' ')                                                  !/*gr4c       25*/
         MCNPwin%flags = MinButton + MaxButton                                  !/*gr4c       26*/
         MCNPwin%x = 0                                                          !/*gr4c       27*/
         MCNPwin%y = 0                                                          !/*gr4c       28*/
         MCNPwin%width = 0                                                      !/*gr4c       29*/
         MCNPwin%height = 0                                                     !/*gr4c       30*/
         MCNPwin%menuid = 0                                                     !/*gr4c       31*/
         MCNPwin%title = "MCNP Plot Window"                                     !/*gr4c       32*/
         call WindowOpen(MCNPwin)                                               !/*gr4c       33*/
         call IGrArea(0.0,0.0,1.0,1.0)                                          !/*gr4c       34*/
         call IGrUnits(0.0,0.0,11.0,8.5)                                        !/*gr4c       35*/
      endif                                                                     !/*gr4c       36*/
#endif                                                                          /*!cpp       341*/
#endif                                                                          /*!grx         8*/
      return                                                                    !/*gr         39*/
      end                                                                       !/*gr         40*/
#if defined(PLOT) || defined(MCPLOT)                                            /*!grxk       10*/
      subroutine rgbval(nc,wm,wx,rg)                                            !/*grxk       11*/
!        set rgb values for visible wavelengths (380-780nm).                    !/*grxk       12*/
!        nc = number of colors.                                                 !/*grxk       13*/
!        wm = minimum wavelength (nm).                                          !/*grxk       14*/
!        wx = maximum wavelength (nm).                                          !/*grxk       15*/
!        r(1-3,i) = red, green, and blue values, ranging from 0 to 1.           !/*grxk       16*/
!                                                                               !/*grxk       17*/
      integer, intent(in) :: nc                                                 !/*grxk       18*/
      real, intent(in) :: wm,wx                                                 !/*grxk       19*/
      real, intent(out) :: rg(3,nc+7)                                           !/*grxk       20*/
!                                                                               !/*grxk       21*/
      real :: wd,wl,r,g,b,s                                                     !/*grxk       22*/
      integer :: i                                                              !/*grxk       23*/
!                                                                               !/*grxk       24*/
      wd=wx-wm                                                                  !/*grxk       25*/
      do i=1,nc                                                                 !/*grxk       26*/
        wl=wm+real(i*wd/nc)                                                     !/*grxk       27*/
        if ((wl.ge.380.).and.(wl.le.440.)) then                                 !/*grxk       28*/
          r=-1.*(wl-440.)/(440.-380.)                                           !/*grxk       29*/
          g=0.                                                                  !/*grxk       30*/
          b=1.                                                                  !/*grxk       31*/
        endif                                                                   !/*grxk       32*/
        if ((wl.ge.440.).and.(wl.le.490.)) then                                 !/*grxk       33*/
          r=0.                                                                  !/*grxk       34*/
          g=(wl-440.)/(490.-440.)                                               !/*grxk       35*/
          b=1.                                                                  !/*grxk       36*/
        endif                                                                   !/*grxk       37*/
        if ((wl.ge.490.).and.(wl.le.510.)) then                                 !/*grxk       38*/
          r=0.                                                                  !/*grxk       39*/
          g=1.                                                                  !/*grxk       40*/
          b=-1.*(wl-510.)/(510.-490.)                                           !/*grxk       41*/
        endif                                                                   !/*grxk       42*/
        if ((wl.ge.510.).and.(wl.le.580.)) then                                 !/*grxk       43*/
          r=(wl-510.)/(580.-510.)                                               !/*grxk       44*/
          g=1.                                                                  !/*grxk       45*/
          b=0.                                                                  !/*grxk       46*/
        endif                                                                   !/*grxk       47*/
        if ((wl.ge.580.).and.(wl.le.645.)) then                                 !/*grxk       48*/
          r=1.                                                                  !/*grxk       49*/
          g=-1.*(wl-645.)/(645.-580.)                                           !/*grxk       50*/
          b=0.                                                                  !/*grxk       51*/
        endif                                                                   !/*grxk       52*/
        if ((wl.ge.645.).and.(wl.le.780.)) then                                 !/*grxk       53*/
          r=1.                                                                  !/*grxk       54*/
          g=0.                                                                  !/*grxk       55*/
          b=0                                                                   !/*grxk       56*/
        endif                                                                   !/*grxk       57*/
!                                                                               !/*grxk       58*/
!        let the intensity fall off near the vision limits.                     !/*grxk       59*/
        if (wl.gt.700.) then                                                    !/*grxk       60*/
          s=.3+.7* (780.-wl)/(780.-700.)                                        !/*grxk       61*/
        else if (wl.lt.420.) then                                               !/*grxk       62*/
          s=.3+.7*(wl-380.)/(420.-380.)                                         !/*grxk       63*/
        else                                                                    !/*grxk       64*/
          s=1.                                                                  !/*grxk       65*/
        endif                                                                   !/*grxk       66*/
!                                                                               !/*grxk       67*/
!        gamma adjust.                                                          !/*grxk       68*/
        rg(1,i)=(s*r)**0.8                                                      !/*grxk       69*/
        rg(2,i)=(s*g)**0.8                                                      !/*grxk       70*/
        rg(3,i)=(s*b)**0.8                                                      !/*grxk       71*/
      enddo                                                                     !/*grxk       72*/
      return                                                                    !/*grxk       73*/
      end                                                                       !/*grxk       74*/
#endif                                                                          /*!grxk       75*/
#if defined(PLOT) || defined(MCPLOT)                                            /*!gr         42*/
      subroutine gxoff(nw)                                                      !/*gr         43*/
!        turn off workstation nw.                                               !/*gr         44*/
      use global_data                                                           !/*grx         9*/
!                                                                               !/*grx        10*/
      implicit real(kindr) (a-h,o-z)                                            !/*grx        11*/
      implicit integer(kindi) (i-n)                                             !/*grx        12*/
!                                                                               !/*grx        13*/
!                                                                               !/*gr         46*/
      if(jgxa(nw).eq.0)return                                                   !/*gr         47*/
      call gdawk(nw)                                                            !/*gr         48*/
      jgxa(nw)=0                                                                !/*gr         49*/
      return                                                                    !/*gr         50*/
      end                                                                       !/*gr         51*/
#endif                                                                          /*!cpp       342*/
#if defined(PLOT) || defined(MCPLOT)                                            /*!gr         53*/
      subroutine gxhome(s,t)                                                    !/*gr         54*/
#ifdef LAHEY                                                                    /*!gr4c       37*/
      use WINTERACTER                                                           !/*gr4c       38*/
#endif                                                                          /*!cpp       344*/
!        return home, update the display, and be ready for fortran i/o.         !/*gr         55*/
      use global_data                                                           !/*grx        14*/
!                                                                               !/*grx        15*/
      implicit real(kindr) (a-h,o-z)                                            !/*grx        16*/
      implicit integer(kindi) (i-n)                                             !/*grx        17*/
!                                                                               !/*grx        18*/
#ifdef LAHEY                                                                    /*!gr4b       18*/
      dimension ia(9)                                                           !/*gr4b       19*/
      real ra(7)                                                                !/*gr4b       20*/
      character hl*40                                                           !/*gr4b       21*/
#endif                                                                          /*!cpp       345*/
!                                                                               !/*gr         57*/
      if(jgxa(1).eq.0)return                                                    !/*gr         58*/
      call gschh(.01*real(wnvp(2)))                                             !/*gr         59*/
#ifndef LAHEY                                                                   /*!gr4b       22*/
      call gtx(real(s),real(t),'.')                                             !/*gr         60*/
#else                                                                           /*!grx        19*/
      call secnd(t1)                                                            !/*gr4b       24*/
      if(ifile.ne.itty)go to 10                                                 !/*gr4b       25*/
      go to 20                                                                  !/*gr4b       29*/
   10 call secnd(t2)                                                            !/*gr4b       30*/
      if(t2-t1.lt.4.d0)go to 10                                                 !/*gr4b       31*/
   20 continue                                                                  !/*gr4c       40*/
#endif                                                                          /*!cpp       347*/
      call guwk(1,0)                                                            !/*gr         61*/
      return                                                                    !/*gr         62*/
      end                                                                       !/*gr         63*/
#endif                                                                          /*!cpp       343*/
#if defined(PLOT) || defined(MCPLOT)                                            /*!gr         65*/
      subroutine gxskip                                                         !/*gr         66*/
!        clear the terminal display and/or advance to the next frame.           !/*gr         67*/
      use global_data                                                           !/*grx        20*/
!                                                                               !/*grx        21*/
      implicit real(kindr) (a-h,o-z)                                            !/*grx        22*/
      implicit integer(kindi) (i-n)                                             !/*grx        23*/
!                                                                               !/*grx        24*/
!                                                                               !/*gr         69*/
      do 10 i=1,2                                                               !/*gr         70*/
      if(jgxa(i).ne.0)call gclrwk(i,1)                                          !/*gr         71*/
   10 if(jgxa(i).ne.0)call guwk(i,1)                                            !/*gr         72*/
      return                                                                    !/*gr         73*/
      end                                                                       !/*gr         74*/
#endif                                                                          /*!cpp       348*/
#if defined(PLOT) || defined(MCPLOT)                                            /*!gr         76*/
      subroutine gxquit                                                         !/*gr         77*/
!        shut down all graphics.                                                !/*gr         78*/
      use global_data                                                           !/*grx        25*/
!                                                                               !/*grx        26*/
      implicit real(kindr) (a-h,o-z)                                            !/*grx        27*/
      implicit integer(kindi) (i-n)                                             !/*grx        28*/
!                                                                               !/*grx        29*/
!                                                                               !/*gr         80*/
      if(jgxa(1).ne.0)call gdawk(1)                                             !/*gr         81*/
      if(jgxa(2).ne.0)call gdawk(2)                                             !/*gr         82*/
      if(jgxo(1).ne.0)call gclwk(1)                                             !/*gr         83*/
      if(jgxo(2).ne.0)call gclwk(2)                                             !/*gr         84*/
      if(jgxo(1)+jgxo(2).ne.0)call gclks                                        !/*gr         85*/
      jgxa(1)=0                                                                 !/*gr         86*/
      jgxa(2)=0                                                                 !/*gr         87*/
      jgxo(1)=0                                                                 !/*gr         88*/
      jgxo(2)=0                                                                 !/*gr         89*/
      return                                                                    !/*gr         90*/
      end                                                                       !/*gr         91*/
#endif                                                                          /*!cpp       349*/
#if defined(PLOT) || defined(MCPLOT)                                            /*!gr         93*/
      subroutine gxlims(a,b,ap,bp,np,jf)                                        !/*gr         94*/
!        define axis limits ap and bp to cover the range from a to b.           !/*gr         95*/
!        b>a is required.  jf=0 for a linear axis, otherwise log10.             !/*gr         96*/
!        if linear, also define the number of divisions np in the range         !/*gr         97*/
!        from 4 to 8.  the increment per division will be 1, 2, or 5.           !/*gr         98*/
!        if log10, ap and bp will be powers of 10.                              !/*gr         99*/
      use global_parameters                                                     !/*grx        30*/
      implicit real(kindr) (a-h,o-z)                                            !/*grx        31*/
      implicit integer(kindi) (i-n)                                             !/*grx        32*/
!                                                                               !/*gr        102*/
      if(jf.ne.0)go to 30                                                       !/*gr        103*/
!                                                                               !/*gr        104*/
!        linear case.                                                           !/*gr        105*/
      e=log10((b-a)/3.99)                                                       !/*gr        106*/
      i=e                                                                       !/*gr        107*/
      if(e.lt.0.)i=i-1                                                          !/*gr        108*/
      f=10.**i                                                                  !/*gr        109*/
      g=2.                                                                      !/*gr        110*/
   10 ai=a/f                                                                    !/*gr        111*/
      bi=b/f                                                                    !/*gr        112*/
      i=ai                                                                      !/*gr        113*/
      if(ai.lt.0..and.i.ne.ai)i=i-1                                             !/*gr        114*/
      j=bi                                                                      !/*gr        115*/
      if(bi.gt.0..and.j.ne.bi)j=j+1                                             !/*gr        116*/
      np=j-i                                                                    !/*gr        117*/
      if(np.le.8)go to 20                                                       !/*gr        118*/
      f=f*g                                                                     !/*gr        119*/
      g=g+.5                                                                    !/*gr        120*/
      go to 10                                                                  !/*gr        121*/
   20 ap=i*f                                                                    !/*gr        122*/
      bp=j*f                                                                    !/*gr        123*/
      return                                                                    !/*gr        124*/
!                                                                               !/*gr        125*/
!        log10 case.                                                            !/*gr        126*/
   30 i=log10(a)                                                                !/*gr        127*/
      if(a.lt.1.)i=i-1                                                          !/*gr        128*/
      j=log10(b)                                                                !/*gr4b       33*/
      if(b.gt.1.)j=j+1                                                          !/*gr4b       34*/
      ap=min(ten**max(i,j-17),half*b)                                           !/*grxl        1*/
      bp=max(ten**j,two*a)                                                      !/*grxl        2*/
      return                                                                    !/*gr        133*/
      end                                                                       !/*gr        134*/
#endif                                                                          /*!cpp       350*/
#if defined(PLOT) || defined(MCPLOT)                                            /*!gr        136*/
      subroutine gxaxis(xm,xx,ym,yx,t,zg,ni,jf,kn,hl)                           !/*gr        137*/
!        draw an axis from xm,ym to xx,yx.  either xm=xx or ym=yx.              !/*gr        138*/
!        add tick marks of length and direction t.                              !/*gr        139*/
!        extend ticks as grid lines to zg.  no grid if zg=0.                    !/*gr        140*/
!        jf=0 for linear (do ni intervals), otherwise log10.                    !/*gr        141*/
!        provide nice intervals if jf=0 and ni=0.                               !/*gr        142*/
!        kn=0 for no numbers, otherwise numbers.                                !/*gr        143*/
!        hl = the axis label.                                                   !/*gr        144*/
      use global_data                                                           !/*grx        33*/
!                                                                               !/*grx        34*/
      implicit real(kindr) (a-h,o-z)                                            !/*grx        35*/
      implicit integer(kindi) (i-n)                                             !/*grx        36*/
!                                                                               !/*grx        37*/
      character hl*(*),hn*10,hp(7)*5                                            !/*gr        146*/
      real xy(2,2),cr(9),zt(2,2)                                                !/*gr        147*/
      data cr/1500.,45.,18.,60.,12.,27.,9.,15.,7.5/                             !/*gr        148*/
      data hp/'0.001','0.01','0.1','1.','10.','100.','1000.'/                   !/*gr        149*/
!                                                                               !/*gr        150*/
      xy(1,1)=xm                                                                !/*gr        151*/
      xy(2,1)=xx                                                                !/*gr        152*/
      xy(1,2)=ym                                                                !/*gr        153*/
      xy(2,2)=yx                                                                !/*gr        154*/
      call gpl(2,xy(1,1),xy(1,2))                                               !/*gr        155*/
      if(t.eq.0..and.zg.eq.0.)return                                            !/*gr        156*/
      k=1                                                                       !/*gr        157*/
      if(xm.eq.xx)k=2                                                           !/*gr        158*/
      zt(2,3-k)=xy(1,3-k)+t                                                     !/*gr        159*/
      ch=.017*wnvp(3-k)*wnvp(2+k)                                               !/*gr        160*/
      w=.0064*wnvp(k)*wnvp(5-k)                                                 !/*gr        162*/
      if(jf.ne.0)go to 140                                                      !/*gr        163*/
!                                                                               !/*gr        164*/
!        add ticks and/or grid lines to linear axis.                            !/*gr        165*/
      ap=xy(1,k)                                                                !/*gr        166*/
      bp=xy(2,k)                                                                !/*gr        167*/
      np=ni                                                                     !/*gr        168*/
      if(ni.ne.0)go to 10                                                       !/*gr        169*/
      a=ap                                                                      !/*gr        170*/
      b=bp                                                                      !/*gr        171*/
      call gxlims(a,b,ap,bp,np,0)                                               !/*gr        172*/
   10 d=(bp-ap)/np                                                              !/*gr        173*/
      zt(1,3-k)=xy(1,3-k)+zg                                                    !/*gr        174*/
      u=xy(1,k)-.0003*(xy(2,k)-xy(1,k))                                         !/*gr        175*/
      v=xy(2,k)+.0003*(xy(2,k)-xy(1,k))                                         !/*gr        176*/
      do 20 i=1,np+1                                                            !/*gr        177*/
      zt(1,k)=ap+d*(i-1)                                                        !/*gr        178*/
      zt(2,k)=zt(1,k)                                                           !/*gr        179*/
      if(zt(1,k).gt.v)go to 30                                                  !/*gr        180*/
   20 if(zt(1,k).ge.u)call gpl(2,zt(1,1),zt(1,2))                               !/*gr        181*/
!                                                                               !/*gr        182*/
!        add minor ticks.                                                       !/*gr        183*/
   30 if(ni.ne.0)go to 50                                                       !/*gr        184*/
      zt(1,3-k)=xy(1,3-k)                                                       !/*gr        185*/
      zt(2,3-k)=xy(1,3-k)+.5*t                                                  !/*gr        186*/
      m=10                                                                      !/*gr        187*/
      if(np.gt.6)m=5                                                            !/*gr        188*/
      e=d/m                                                                     !/*gr        189*/
      do 40 i=1,np                                                              !/*gr        190*/
      a=ap+d*(i-1)                                                              !/*gr        191*/
      do 40 j=1,m-1                                                             !/*gr        192*/
      zt(1,k)=a+e*j                                                             !/*gr        193*/
      zt(2,k)=zt(1,k)                                                           !/*gr        194*/
      if(zt(1,k).gt.v)go to 50                                                  !/*gr        195*/
   40 if(zt(1,k).ge.u)call gpl(2,zt(1,1),zt(1,2))                               !/*gr        196*/
!                                                                               !/*gr        197*/
!        print numbers on linear axis.                                          !/*gr        198*/
   50 if(kn.eq.0)return                                                         !/*gr        199*/
      if(k.eq.2)call gschup(-1.,0.)                                             !/*gr        200*/
      call gschh(.8*real(ch))                                                   !/*gr4b       37*/
      if(k.eq.2)call gschxp(real(wnvp(2)*wnvp(3)/(wnvp(1)*wnvp(4))))            !/*gr        201*/
      kf=3                                                                      !/*gr        202*/
      a=max(abs(ap),abs(bp))                                                    !/*gr        203*/
      c=1.                                                                      !/*gr        204*/
      if(min(ap,bp).lt.0.)c=.1                                                  !/*gr        205*/
      if(a.le.9999.*c.and.a.ge..1)kf=2                                          !/*gr        206*/
      j=bp-ap                                                                   !/*gr        207*/
      if(a.le.99999.*c.and.aint(ap).eq.ap.and.aint(bp).eq.bp.and.               !/*gr        208*/
     & np*(j/np).eq.j)kf=1                                                      !/*gr        209*/
      j=log10(a)                                                                !/*gr        210*/
      if(a.lt.1..and.a.ne.10.**j)j=j-1                                          !/*gr        211*/
      f=1./10.**j                                                               !/*gr        212*/
      do 120 l=1,np+1                                                           !/*gr        213*/
      a=ap+(l-1)*d                                                              !/*gr        214*/
      if(a.gt.v)go to 130                                                       !/*gr        215*/
      if(a.lt.u)go to 120                                                       !/*gr        216*/
      go to(60,70,80)kf                                                         !/*gr        217*/
   60 write(hn,'(i6)')nint(a)                                                   !/*gr        218*/
      go to 110                                                                 !/*gr        219*/
   70 write(hn,'(f10.4)')a                                                      !/*gr        220*/
      go to 90                                                                  !/*gr        221*/
   80 write(hn,'(f10.3)')a*f                                                    !/*gr        222*/
   90 do 100 i=1,4                                                              !/*gr        223*/
      if(hn(11-i:11-i).ne.'0')go to 110                                         !/*gr        224*/
  100 hn(11-i:11-i)=' '                                                         !/*gr        225*/
  110 call ljusti(hn)                                                           !/*gr        226*/
      hn(6:10)=' '                                                              !/*gr        227*/
      i=leng(hn)                                                                !/*gr        228*/
      if(k.eq.1)call gtx(real(a-w*i),real(ym+2.*t-ch),hn(1:i))                  !/*gr        229*/
      if(k.eq.2)call gtx(real(xm+2.*t),real(a-w*i),hn(1:i))                     !/*gr        230*/
  120 continue                                                                  !/*gr        231*/
  130 if(kf.ne.3.or.j.eq.0)go to 190                                            !/*gr        232*/
      write(hn,'(i3)')j                                                         !/*gr        233*/
      call ljusti(hn)                                                           !/*gr        234*/
      i=leng(hn)                                                                !/*gr        235*/
      if(k.eq.1)call gtx(real(xx+6.*w),real(ym+2.*t-ch),'e'//hn(1:i))           !/*gr        236*/
      if(k.eq.2)call gtx(real(xm+2.5*t-ch),real(yx-i*w),'e'//hn(1:i))           !/*gr        237*/
      go to 190                                                                 !/*gr        238*/
!                                                                               !/*gr        239*/
!        add ticks to log10 axis.                                               !/*gr        240*/
  140 m=xy(1,k)                                                                 !/*gr        241*/
      if(xy(1,k).lt.0..and.m.ne.xy(1,k))m=m-1                                   !/*gr        242*/
      n=xy(2,k)                                                                 !/*gr        243*/
      if(xy(2,k).gt.0..and.n.ne.xy(2,k))n=n+1                                   !/*gr        244*/
      d=xy(2,k)-xy(1,k)                                                         !/*gr        245*/
      x=10.**m                                                                  !/*gr        246*/
      xl=10.**xy(1,k)                                                           !/*gr        247*/
      xu=10.**xy(2,k)                                                           !/*gr        248*/
      do 160 i=1,n-m+1                                                          !/*gr        249*/
      dx=x                                                                      !/*gr        250*/
      do 150 j=1,9                                                              !/*gr        251*/
      if(x.gt.xu*(1.+.001*d))go to 170                                          !/*gr        252*/
      if(x.lt.xl*(1.-.001*d).or.cr(j).lt.d)go to 150                            !/*gr        253*/
      zt(1,3-k)=xy(1,3-k)                                                       !/*gr        254*/
      if(j.eq.1)zt(1,3-k)=xy(1,3-k)+zg                                          !/*gr        255*/
      zt(1,k)=log10(x)                                                          !/*gr        256*/
      zt(2,k)=zt(1,k)                                                           !/*gr        257*/
      call gpl(2,zt(1,1),zt(1,2))                                               !/*gr        258*/
  150 x=x+dx                                                                    !/*gr        259*/
  160 continue                                                                  !/*gr        260*/
!                                                                               !/*gr        261*/
!        print numbers on log10 axis.                                           !/*gr        262*/
  170 if(kn.eq.0)return                                                         !/*gr        263*/
      if(k.eq.2)call gschup(-1.,0.)                                             !/*gr        264*/
      call gschh(.8*real(ch))                                                   !/*gr4b       38*/
      if(k.eq.2)call gschxp(real(wnvp(2)*wnvp(3)/(wnvp(1)*wnvp(4))))            !/*gr        265*/
      do 180 i=m,n                                                              !/*gr        266*/
      if(i.gt.xy(2,k)+.001*d)go to 190                                          !/*gr        267*/
      if(i.lt.xy(1,k)-.001*d)go to 180                                          !/*gr        268*/
      write(hn,'(2h10,sp,i4)')i                                                 !/*gr        269*/
      call ljusti(hn(3:10))                                                     !/*gr        270*/
      if(i.ge.-3.and.i.le.3)hn=hp(i+4)                                          !/*gr        271*/
      j=leng(hn)                                                                !/*gr        272*/
      if(k.eq.1)call gtx(real(i-w*j),real(ym+2.*t-ch),hn(1:j))                  !/*gr        273*/
      if(k.eq.2)call gtx(real(xm+2.*t),real(i-w*j),hn(1:j))                     !/*gr        274*/
  180 continue                                                                  !/*gr        275*/
!                                                                               !/*gr        276*/
!        print the axis label, if any.                                          !/*gr        277*/
  190 if(hl.eq.' ')go to 200                                                    !/*gr        278*/
      j=leng(hl)                                                                !/*gr        279*/
      call gschh(real(ch))                                                      !/*gr        280*/
      w=1.25*w                                                                  !/*gr        281*/
#if defined(LAHEY) || defined(QWIN)                                             /*!dp4c2       3*/
      if(k.eq.1)call gtx(real(.5*(xm+xx)-w*j),                                  !/*dp4c2       4*/
     & real(ym+2.5*t-3.*ch),hl(1:j))                                            !/*dp4c2       5*/
#else                                                                           /*!grx        38*/
      if(k.eq.1)call gtx(real(.5*(xm+xx)-w*j),                                  !/*dp4c2       7*/
     & real(ym+2.5*t-2.2*ch),hl(1:j))                                           !/*dp4c2       8*/
#endif                                                                          /*!cpp       354*/
      if(k.eq.2)call gtx(real(xm+2.5*t-1.2*ch),                                 !/*gr        284*/
     & real(.5*(ym+yx)-w*j),hl(1:j))                                            !/*gr        285*/
  200 if(k.eq.2)call gschup(0.,1.)                                              !/*gr        286*/
      if(k.eq.2)call gschxp(real(wnvp(1)*wnvp(4)/(wnvp(2)*wnvp(3))))            !/*gr        287*/
      return                                                                    !/*gr        288*/
      end                                                                       !/*gr        289*/
#endif                                                                          /*!cpp       352*/
