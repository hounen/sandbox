!#ident gmgww.F from deck gm                                                    !/*gm          1*/
      subroutine gmgww                                                          !/*gm          2*/
!        smooth the multigroup fluxes and calculate weight windows.             !/*gm          3*/
      use global_data                                                           !/*gmx         1*/
!                                                                               !/*gmx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*gmx         3*/
      implicit integer(kindi) (i-n)                                             !/*gmx         4*/
!                                                                               !/*gmx         5*/
!                                                                               !/*gm          5*/
      write(iuo,10)                                                             !/*gm          6*/
   10 format(43h1weight-window cards from multigroup fluxes,                    !/*gm          7*/
     & 61x,15hprint table 198//10x,25heach card has ten leading,                !/*gm4a        1*/
     & 46h blanks that must be removed by a text editor./)                      !/*gm          9*/
      ir=namchg(1,icw)                                                          !/*gm         10*/
      do 150 ip=1,mipt                                                          !/*gm         11*/
      if(jgm(ip).eq.0)go to 150                                                 !/*gm         12*/
!                                                                               !/*gm         13*/
!        set importances to flux values = flx/vol.                              !/*gm4a        2*/
      do 20 ia=1,mxa                                                            !/*gm         15*/
      do 20 jg=1,jgm(ip)                                                        !/*gm         16*/
      scr(lscr+mxa*(jg-1)+ia)=0.                                                !/*gm4b        1*/
   20 if(fim(lfim+ip,ia)*vol(lvol+ia).gt.0.)scr(lscr+mxa*(jg-1)+ia)             !/*gm4b        2*/
     & =max(flx(lflx+ia+mxa*(jg-1+mgm(ip)))/vol(lvol+ia),zero+1.e-30)           !/*gm4b        3*/
!                                                                               !/*gm         20*/
!        smooth flux values over five adjacent energy groups.                   !/*gm         21*/
      do 50 i=1,mxa                                                             !/*gm         22*/
      a=0.                                                                      !/*gm         23*/
      do 40 j=1,jgm(ip)                                                         !/*gm         24*/
      t=0.                                                                      !/*gm         25*/
      a=a+scr(lscr+mxa*(j-1)+i)                                                 !/*gm         26*/
      do 30 k=max(1,j-2),min(jgm(ip),j+2)                                       !/*gm         27*/
   30 t=t+scr(lscr+mxa*(k-1)+i)                                                 !/*gm         28*/
   40 scr(lscr+mxa*(j-1)+i)=max(scr(lscr+mxa*(j-1)+i),.01*t)                    !/*gm         29*/
      a=.001*a/jgm(ip)                                                          !/*gm         30*/
      do 50 j=1,jgm(ip)                                                         !/*gm         31*/
   50 scr(lscr+mxa*(j-1)+i)=max(scr(lscr+mxa*(j-1)+i),a)                        !/*gm         32*/
!                                                                               !/*gm         33*/
!        second pass energy smoothing of flux values.                           !/*gm         34*/
      do 70 ia=1,mxa                                                            !/*gm         35*/
      k=1                                                                       !/*gm         36*/
      do 60 j=2,jgm(ip)                                                         !/*gm         37*/
   60 if(scr(lscr+mxa*(j-1)+ia).gt.scr(lscr+mxa*(k-1)+ia))k=j                   !/*gm         38*/
      do 70 j=1,jgm(ip)                                                         !/*gm         39*/
      l=lscr+mxa*(k-j-1)+ia                                                     !/*gm         40*/
      if(k-j.ge.1)scr(l)=max(scr(l),.2*scr(l+mxa))                              !/*gm4a        4*/
      l=lscr+mxa*(k+j-1)+ia                                                     !/*gm         42*/
   70 if(k+j.le.jgm(ip))scr(l)=max(scr(l),.2*scr(l-mxa))                        !/*gm4a        5*/
!                                                                               !/*gm         44*/
!        calculate weight windows.                                              !/*gm         45*/
      e=0.                                                                      !/*gm         46*/
      do 80 i=1,jgm(ip)                                                         !/*gm         47*/
   80 e=max(e,scr(lscr+mxa*(i-1)+ir))                                           !/*gm         48*/
      if(e.eq.0.)e=1.                                                           !/*gm         49*/
      if(fnw.ne.0.)e=e*fnw                                                      !/*gm         50*/
      r=3.                                                                      !/*gm         51*/
      if(rim.ne.0.)r=log10(rim)                                                 !/*gm         52*/
      do 100 jg=1,jgm(ip)                                                       !/*gm         53*/
      tm=0.                                                                     !/*gm         54*/
      ts=huge                                                                   !/*gm         55*/
      do 90 ia=1,mxa                                                            !/*gm         56*/
      if(fim(lfim+ip,ia).eq.0.)go to 90                                         !/*gm         57*/
      t=scr(lscr+mxa*(jg-1)+ia)                                                 !/*gm         58*/
      tm=max(tm,t)                                                              !/*gm         59*/
      if(t.gt.0.)ts=min(ts,t)                                                   !/*gm         60*/
   90 continue                                                                  !/*gm         61*/
      pm=min(one,r/(max(one,log10(tm/ts))))                                     !/*gm         62*/
      do 100 ia=1,mxa                                                           !/*gm         63*/
      if(fim(lfim+ip,ia).ne.0..and.scr(lscr+mxa*(jg-1)+ia).ne.0.)               !/*gm         64*/
     & scr(lscr+mxa*(jg-1)+ia)=(e/scr(lscr+mxa*(jg-1)+ia))**pm                  !/*gm         65*/
  100 if(fim(lfim+ip,ia).eq.0.)scr(lscr+mxa*(jg-1)+ia)=-1.                      !/*gm         66*/
!                                                                               !/*gm         67*/
!        write weight-window cards on the outp file.                            !/*gm         68*/
      ng=jgm(ip)                                                                !/*gm         69*/
      write(iuo,110)hip(ip:ip)                                                  !/*gm         70*/
  110 format(10x,4hwwp:,a1,6h 5 3 5)                                            !/*gm         71*/
      j=jxs(ljxs+1,mgegbt(ip))+ng                                               !/*gm         72*/
      write(iuo,120)hip(ip:ip),(xss(j-i)+.5*xss(j+jgm(ip)-i),i=1,ng)            !/*gm         73*/
  120 format(10x,4hwwe:,a1,1p5e12.4:/(15x,5e12.4))                              !/*gm         74*/
      do 130 ie=1,ng                                                            !/*gm         75*/
      j1=ie/10                                                                  !/*gm4c2       1*/
      j2=mod(ie,10)                                                             !/*gm4c2       2*/
  130 write(iuo,140)j1,j2,hip(ip:ip),(scr(lscr+mxa*(ng-ie)+i),i=1,mxa)          !/*gm4c2       3*/
  140 format(10x,3hwwn,2i1,1h:,a1,1x,1p5e12.4:/(18x,5e12.4))                    !/*gm4c2       4*/
  150 continue                                                                  !/*gm         78*/
      return                                                                    !/*gm         79*/
      end                                                                       !/*gm         80*/
