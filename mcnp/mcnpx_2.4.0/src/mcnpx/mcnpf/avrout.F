!#ident avrout.F from deck vz4c                                                 !/*vz4c        1*/
      subroutine avrout                                                         !/*vz4c        2*/
!        mesh-based weight window generator.                                    !/*vz4c        3*/
!        normalize windows and write wwout and wwone files.                     !/*vz4c        4*/
      use global_data                                                           !/*vzx         1*/
!                                                                               !/*vzx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*vzx         3*/
      implicit integer(kindi) (i-n)                                             !/*vzx         4*/
!                                                                               !/*vzx         5*/
      character ha*6,hb*7                                                       !/*vz4c2       1*/
!                                                                               !/*vz4c        7*/
!        copy wwfa into scratch file, to be restored for continue runs.         !/*vz4c        8*/
      open(iu3,status='scratch',form='unformatted')                             !/*vz4c        9*/
      write(iu3)(wwfa(lwfa+i),i=1,(mgww(mipt+1)+mipt)*nwwma)                    !/*vz4c       10*/
!                                                                               !/*vz4c       11*/
!        find normalization constant and normalize to maximum importance        !/*vz4c       12*/
!        in all energy groups in source cell.                                   !/*vz4c       13*/
!        the source particle is used for normalization (nips).                  !/*vz4c       14*/
      fs=0.                                                                     !/*vz4c       15*/
      ei=0.                                                                     !/*vz4c       16*/
      do 10 i=1,ngww(nips)                                                      !/*vz4c       17*/
      i1=mai+nwwma*(i-1+mgww(nips))                                             !/*vz4c       18*/
      if(wwfa(lwfa+i1).gt.0.)ei=swwfa(lsww+i1)/wwfa(lwfa+i1)                    !/*vz4c       19*/
   10 fs=max(fs,ei)                                                             !/*vz4c       20*/
      cn=wwg(3)*fs                                                              !/*vz4c       21*/
      if(cn.eq.0.)cn=.5*pax(2,1,nips)*fpi*fs                                    !/*vz4c       22*/
!                                                                               !/*vz4c       23*/
!        get normalization constant for single-group output.                    !/*vz4c       24*/
      c1=0.                                                                     !/*vz4c       25*/
      ft=0.                                                                     !/*vz4c       26*/
      i2=mai+nwwma*(nips-1+mgww(mipt+1))                                        !/*vz4c       27*/
      if(wwfa(lwfa+i2).gt.0.)ft=swwfa(lsww+i2)/wwfa(lwfa+i2)                    !/*vz4c       28*/
      c1=wwg(3)*ft                                                              !/*vz4c       29*/
      if(c1.eq.0.)c1=.5*pax(2,1,nips)*fpi*ft                                    !/*vz4c       30*/
!                                                                               !/*vzx         6*/
!        write normalization constant for generated mesh windows.               !/*vzx         7*/
      if(ink(190).ne.0.and.nwgeoa.lt.3)write(iuo,15)ngww(nips),cn,c1            !/*vzx         8*/
   15 format(35h1mesh-based weight window generator,54x,                        !/*vzx         9*/
     & 15hprint table 190//5x,14hnormalization:,i4,7h-group:,                   !/*vzx        10*/
     & 1p1e12.4,10x,8h1-group:,e12.4//)                                         !/*vzx        11*/
!                                                                               !/*vz4c       31*/
!        write the weight window lower bounds to the wwout file.                !/*vz4c       32*/
      do 150 ip=1,mipt                                                          !/*vz4c       33*/
      if(ngww(ip).eq.0)go to 150                                                !/*vz4c       34*/
!                                                                               !/*vz4c       35*/
!        normalize single group and energy-dependent weight windows.            !/*vz4c       36*/
      do 20 i=1,nwwma                                                           !/*vz4c       37*/
      i2=i+nwwma*(ip-1+mgww(mipt+1))                                            !/*vz4c       38*/
      s=0.                                                                      !/*vz4c       39*/
      if(swwfa(lsww+i2).ne.0.)s=c1*wwfa(lwfa+i2)/swwfa(lsww+i2)                 !/*vz4c       40*/
      wwfa(lwfa+i2)=s                                                           !/*vz4c       41*/
      do 20 ng=1,ngww(ip)                                                       !/*vz4c       42*/
      i1=i+nwwma*(ng-1+mgww(ip))                                                !/*vz4c       43*/
      w=0.                                                                      !/*vz4c       44*/
      if(swwfa(lsww+i1).ne.0.)w=cn*wwfa(lwfa+i1)/swwfa(lsww+i1)                 !/*vz4c       45*/
   20 wwfa(lwfa+i1)=w                                                           !/*vz4c       46*/
!                                                                               !/*vz4c       47*/
!        write mesh-based windows to wwout file.                                !/*vz4c       48*/
      if(nwgeoa.eq.3)go to 40                                                   !/*vz4c       49*/
      write(iuwe,'(6g13.5)')(ewwg(lewg+i+mgww(ip)),i=1,ngww(ip))                !/*vz4c       50*/
      do 30 ng=1,ngww(ip)                                                       !/*vz4c       51*/
   30 write(iuwe,'(6g13.5)')(wwfa(lwfa+i+nwwma*(ng-1+mgww(ip))),                !/*vz4c       52*/
     & i=1,nwwma)                                                               !/*vz4c       53*/
!                                                                               !/*vz4c       54*/
!        write mesh-based windows to wwone file.                                !/*vz4c       55*/
      if(ngww(ip).eq.1)go to 150                                                !/*vz4c       56*/
      write(iuw1,'(6g13.5)')ewwg(lewg+ngww(ip)+mgww(ip))                        !/*vz4c       57*/
      write(iuw1,'(6g13.5)')(wwfa(lwfa+i+nwwma*(ip-1+mgww(mipt+1))),            !/*vz4c       58*/
     & i=1,nwwma)                                                               !/*vz4c       59*/
      go to 150                                                                 !/*vz4c       60*/
!                                                                               !/*vz4c       61*/
!        cell-based windows: set zero importance regions equal to -1.           !/*vz4c       62*/
   40 do 60 i=1,nwwma                                                           !/*vz4c       63*/
      if(fim(lfim+ip,i).ne.0.)go to 60                                          !/*vz4c       64*/
      wwfa(lwfa+i+nwwma*(ip-1+mgww(mipt+1)))=-1.                                !/*vz4c       65*/
      do 50 ng=1,ngww(ip)                                                       !/*vz4c       66*/
   50 wwfa(lwfa+i+nwwma*(ng-1+mgww(ip)))=-1.                                    !/*vz4c       67*/
   60 continue                                                                  !/*vz4c       68*/
!                                                                               !/*vz4c       69*/
!        write cell-based windows to wwout file.                                !/*vz4c       70*/
      write(iuwe,80)hip(ip:ip),(ewwg(lewg+i+mgww(ip)),i=1,ngww(ip))             !/*vz4c       71*/
      do 70 ng=1,ngww(ip)                                                       !/*vz4c       72*/
      if(ng.lt.10)write(hb,'(3hwwn,i1,1h:,a1,1h )')ng,hip(ip:ip)                !/*vz4c2      12*/
      if(ng.ge.10)write(hb,'(3hwwn,i2,1h:,a1)')ng,hip(ip:ip)                    !/*vz4c2      13*/
   70 write(iuwe,90)hb,(wwfa(lwfa+i+nwwma*(ng-1+mgww(ip))),i=1,nwwma)           !/*vz4c2      14*/
!                                                                               !/*vz4c       75*/
!        write cell-based windows to wwone file.                                !/*vz4c       76*/
      if(ngww(ip).eq.1)go to 100                                                !/*vz4c       77*/
      write(iuw1,80)hip(ip:ip),ewwg(lewg+ngww(ip)+mgww(ip))                     !/*vz4c       78*/
   80 format(4hwwe:,a1,1p5e12.4/(5x,5e12.4))                                    !/*vz4c       79*/
      write(hb,'(5hwwn1:,a1,1h )')hip(ip:ip)                                    !/*vz4c2      15*/
      write(iuw1,90)hb,                                                         !/*vz4c2      16*/
     & (wwfa(lwfa+i+nwwma*(ip-1+mgww(mipt+1))),i=1,nwwma)                       !/*vz4c2      17*/
   90 format(a7,1x,1p5e12.4/(8x,5e12.4))                                        !/*vz4c2      18*/
!                                                                               !/*vz4c       84*/
!        print a table of the weight window lower bounds.                       !/*vz4c       85*/
  100 write(iuo,110)hnp(ip)                                                     !/*vz4c       86*/
  110 format(1h1,a11,36h weight-window lower bounds from the,                   !/*vzx        12*/
     & 24h weight-window generator,32x,15hprint table 190)                      !/*vzx        13*/
      ha='energy'                                                               !/*vz4c       89*/
      if(wwg(8).ne.0.)ha='  time'                                               !/*vz4c       90*/
      do 130 k=1,ngww(ip),10                                                    !/*vz4c       91*/
      l=min(k+9,ngww(ip))                                                       !/*vz4c       92*/
      if(ngww(ip).gt.1.or.ewwg(lewg+1+mgww(ip)).ne.0.)                          !/*vz4c       93*/
     & write(iuo,120)ha,(ewwg(lewg+j+mgww(ip)),j=k,l)                           !/*vz4c       94*/
  120 format(/1x,a6,2h: ,1p10e11.3)                                             !/*vz4c       95*/
      write(iuo,'(6h  cell)')                                                   !/*vz4c       96*/
      do 130 i=1,nwwma                                                          !/*vz4c       97*/
  130 write(iuo,140)ncl(lncl+i),                                                !/*vz4c       98*/
     & (wwfa(lwfa+i+nwwma*(ie-1+mgww(ip))),ie=k,l)                              !/*vz4c       99*/
  140 format(i6,3x,1p10e11.3)                                                   !/*vz4c      100*/
  150 continue                                                                  !/*vz4c      101*/
!                                                                               !/*vz4c      102*/
!        error messages for mesh-based weight window generator.                 !/*vz4c      103*/
      if(nwgeoa.eq.3)go to 160                                                  !/*vz4c      104*/
      if(nsom.ne.0)write(jtty,                                                  !/*vz4c      105*/
     & '(/i10,36h particles started outside wwg mesh.)')nsom                    !/*vz4c      106*/
      if(nsom.ne.0)write(iuo,                                                   !/*vz4c      107*/
     & '(/i10,36h particles started outside wwg mesh.)')nsom                    !/*vz4c      108*/
      if(nesm.eq.0)go to 260                                                    !/*vz4c      109*/
      write(jtty,'(/i10,28h particles escaped wwg mesh.)')nesm                  !/*vz4c      110*/
      write(iuo,'(/i10,28h particles escaped wwg mesh.)')nesm                   !/*vz4c      111*/
      go to 260                                                                 !/*vz4c      112*/
!                                                                               !/*vz4c      113*/
!        write cell-based weight window cards on the outp file.                 !/*vz4c      114*/
  160 write(iuo,170)                                                            !/*vz4c      115*/
  170 format(53h1weight-window cards from the weight-window generator,          !/*vz4c      116*/
     & 51x,15hprint table 200//10x,25heach card has ten leading,                !/*vz4c      117*/
     & 46h blanks that must be removed by a text editor./)                      !/*vz4c      118*/
      ij=0                                                                      !/*vz4c      119*/
      if(wwg(8).ne.0.)ij=1                                                      !/*vz4c      120*/
      do 220 ip=1,mipt                                                          !/*vz4c      121*/
      if(ngww(ip).eq.0)go to 220                                                !/*vz4c      122*/
      write(iuo,180)hip(ip:ip),ij                                               !/*vz4c      123*/
  180 format(10x,4hwwp:,a1,10h 5 3 5 0 0,i2)                                    !/*vz4c      124*/
      if(ngww(ip).gt.1.or.ewwg(lewg+1+mgww(ip)).ne.0.)                          !/*vz4c      125*/
     & write(iuo,190)hip(ip:ip),(ewwg(lewg+i+mgww(ip)),i=1,ngww(ip))            !/*vz4c      126*/
  190 format(10x,4hwwe:,a1,1p5e12.4/(15x,5e12.4))                               !/*vz4c      127*/
      do 200 ie=1,ngww(ip)                                                      !/*vz4c      128*/
      if(ie.lt.10)write(hb,'(3hwwn,i1,1h:,a1,1h )')ie,hip(ip:ip)                !/*vz4c2      19*/
      if(ie.ge.10)write(hb,'(3hwwn,i2,1h:,a1)')ie,hip(ip:ip)                    !/*vz4c2      20*/
  200 write(iuo,210)hb,(wwfa(lwfa+i+nwwma*(ie-1+mgww(ip))),i=1,nwwma)           !/*vz4c2      21*/
  210 format(10x,a7,1x,1p5e12.4/(18x,5e12.4))                                   !/*vz4c2      22*/
  220 continue                                                                  !/*vz4c      132*/
!                                                                               !/*vz4c      133*/
!        write cell-based source energy/time bias cards on outp.                !/*vz4c      134*/
      if(ngww(nips).le.1.or.cn.eq.0.)go to 250                                  !/*vz4c      135*/
      write(iuo,230)ha,ecf(nips),(ewwg(lewg+ie+mgww(nips)),                     !/*vz4c      136*/
     & ie=1,ngww(nips))                                                         !/*vz4c      137*/
  230 format(///8h source ,a6,30h bias cards from the generated,                !/*vz4c      138*/
     & 15h weight window.//10x,4hsi99,1x,1p5e12.4/(15x,5e12.4))                 !/*vz4c      139*/
      write(iuo,240)zero,(cn/max(zero+1e-30,                                    !/*vz4c      140*/
     & wwfa(lwfa+mai+nwwma*(ie-1+mgww(nips)))),ie=1,ngww(nips))                 !/*vz4c      141*/
  240 format(10x,4hsb99,1x,1p5e12.4/(15x,5e12.4))                               !/*vz4c      142*/
!                                                                               !/*vz4c      143*/
!        print a table of cells with excessive importance ratios.               !/*vz4c      144*/
  250 call crtcze(1)                                                            !/*vz4c      145*/
!                                                                               !/*vz4c      146*/
!        restore wwfa from scratch file for continue runs.                      !/*vz4c      147*/
  260 endfile iu3                                                               !/*vz4c      148*/
      rewind iu3                                                                !/*vz4c      149*/
      read(iu3)(wwfa(lwfa+i),i=1,(mgww(mipt+1)+mipt)*nwwma)                     !/*vz4c      150*/
      close(iu3)                                                                !/*vz4c      151*/
      return                                                                    !/*vz4c      152*/
      end                                                                       !/*vz4c      153*/
