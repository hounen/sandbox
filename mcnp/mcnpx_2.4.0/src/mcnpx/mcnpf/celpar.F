!#ident celpar.F from deck ca                                                   !/*ca          1*/
      subroutine celpar(mm)                                                     !/*ca          2*/
!        extract and check any cell-parameter card names in the cell            !/*ca          3*/
!        card in klin.  mm=1 for pass1, 2 for rdprob.                           !/*ca          4*/
      use global_data                                                           !/*cax         1*/
      use imcn_data                                                             !/*cax         2*/
!                                                                               !/*cax         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*cax         4*/
      implicit integer(kindi) (i-n)                                             !/*cax         5*/
!                                                                               !/*cax         6*/
      character ht*5,hp*5,hn*5,hf*36                                            !/*ca          7*/
!                                                                               !/*ca          8*/
!        check for continue and determine where to start reading klin.          !/*ca4a        1*/
      iu=0                                                                      !/*ca4a        2*/
      ht=' '                                                                    !/*ca4a        3*/
      if(klin(1:5).eq.' '.or.irc.lt.0)ht='&'                                    !/*ca4a        4*/
      if(ht.eq.' ')call nxtsym(klin,' ',1,it,iu,1)                              !/*ca4a        5*/
!                                                                               !/*ca4a        6*/
!        write card to scratch file (& = continue).                             !/*ca4a        7*/
      write(iu1,'(a5,a80)')ht,klin                                              !/*ca4a        8*/
!                                                                               !/*ca4a        9*/
!        extract cell parameter type.                                           !/*ca4a       10*/
   10 call nxtsym(klin,' $():#,=&',iu+1,it,iu,1)                                !/*ca4a       11*/
      if(iu.eq.0)return                                                         !/*ca         13*/
      if(klin(it:it).eq.'$')return                                              !/*ca         14*/
      if(klin(iu:iu).eq.'&')return                                              !/*ca4a       12*/
      if(index('():#,=',klin(it:it)).ne.0)go to 10                              !/*ca         15*/
      if(kdata(klin(it:iu)).ne.0)go to 10                                       !/*ca         16*/
      if((it.eq.iu.or.kdata(klin(it:max(it,iu-1))).ne.0).and.                   !/*ca         17*/
     & index('rijm',klin(iu:iu)).ne.0)go to 10                                  !/*ca         18*/
      if(klin(it:iu).eq.'like'.or.klin(it:iu).eq.'but'.or.                      !/*ca         19*/
     & klin(it:iu).eq.'mat'.or.klin(it:iu).eq.'rho')go to 10                    !/*ca         20*/
      i=it                                                                      !/*ca         21*/
      call nxtsym(klin,' =',i,it,iu,1)                                          !/*ca4a       13*/
      n=0                                                                       !/*ca         24*/
      hn=' '                                                                    !/*ca         25*/
      hp=' '                                                                    !/*ca         26*/
      do 30 ij=it,iu                                                            !/*caxk        1*/
      ht=klin(ij:ij)                                                            !/*caxk        2*/
      if(ht.eq.':')go to 40                                                     !/*ca         29*/
      if(ij.gt.it+4)go to 60                                                    !/*caxk        3*/
      if(index('abcdefghijklmnopqrstuvwxyz',ht(1:1)).eq.0)go to 20              !/*ca         31*/
      n=n+1                                                                     !/*ca         32*/
      hp(n:n)=ht(1:1)                                                           !/*ca         33*/
      go to 30                                                                  !/*ca         34*/
   20 ht=hn(2:5)//klin(ij:ij)                                                   !/*caxk        4*/
      if(index('0123456789',ht(5:5)).ne.0)hn=ht                                 !/*ca         36*/
   30 continue                                                                  !/*ca         37*/
   40 read(hn,'(i5)')n                                                          !/*ca         38*/
      do 50 j=3,nkcd                                                            !/*ca         40*/
   50 if(cnm(j).eq.hp)go to 60                                                  !/*ca         41*/
      if(mm.ne.1)call erprnt(2,1,0,0,0,0,0,0,                                   !/*ca4a       16*/
     & '33h'//hp//' is not a legal data symbol.')                               !/*ca4a       17*/
      go to 10                                                                  !/*ca4a       18*/
   60 if(mm.ne.1)go to 110                                                      !/*ca4a       19*/
!                                                                               !/*ca         43*/
!        pass1 case.                                                            !/*ca         44*/
      if(krq(7,j).ne.1)go to 10                                                 !/*ca         46*/
      if(ij.le.iu.and.ij.gt.it+4.and.klin(ij:ij).ne.':')go to 10                !/*caxk        5*/
      mp=1                                                                      !/*caxk        6*/
   70 if(ij.lt.iu)mp=index(hip,klin(ij+1:ij+1))                                 !/*caxk        7*/
      if(hp.eq.'tmp'.or.hp.eq.'wwn')n=max(1,n)                                  !/*ca         50*/
      if(hp.ne.'pd')go to 90                                                    !/*ca         51*/
      if(n.eq.0)go to 10                                                        !/*ca         52*/
      i=n                                                                       !/*ca         53*/
      do 80 n=1,ncpar(mp,j)                                                     !/*ca         54*/
   80 if(i.eq.ndp(n))go to 90                                                   !/*ca         55*/
      ndp(n)=i                                                                  !/*ca         56*/
   90 k=max(ncpar(mp,j),n)                                                      !/*ca         57*/
      if(k.eq.0)k=-1                                                            !/*ca         58*/
      ncparf=ncparf+abs(k-ncpar(mp,j))                                          !/*ca         59*/
      ncpar(mp,j)=k                                                             !/*ca         60*/
      if(hp.eq.'tmp')mxt=max(mxt,n)                                             !/*ca         62*/
      if(ij.ge.iu)go to 10                                                      !/*caxk        8*/
      if(hp.eq.'imp')ifip(mp)=1                                                 !/*caxk        9*/
      if(hp.eq.'wwn')nww(mp)=max(nww(mp),n)                                     !/*caxk       10*/
      ij=ij+2                                                                   !/*caxk       11*/
      if(ij.ge.iu)go to 10                                                      !/*caxk       12*/
      go to 70                                                                  !/*ca4a       25*/
!                                                                               !/*ca         65*/
!        rdprob case.                                                           !/*ca         66*/
  110 if(krq(7,j).eq.1)go to 120                                                !/*ca         71*/
      call erprnt(2,1,0,0,0,0,0,0,                                              !/*ca         72*/
     & '42h'//hp//' is not a cell-parameter data symbol.')                      !/*ca         73*/
      go to 10                                                                  !/*ca         74*/
  120 if(ij.gt.iu)go to 130                                                     !/*caxk       13*/
      if(ij.gt.it+4.and.klin(ij:ij).ne.':')go to 140                            !/*caxk       14*/
  130 if(krq(2,j).eq.0.and.ij.ne.iu+1)go to 140                                 !/*caxk       15*/
      if(krq(2,j).ne.0.and.ij.ge.iu)call erprnt(2,1,0,0,0,0,0,0,                !/*caxk       16*/
     & '46hparticle-type designator is missing from '//hp)                      !/*ca         79*/
      go to 10                                                                  !/*ca         80*/
  140 hf=klin(it:iu)                                                            !/*ca         81*/
      call erprnt(2,1,0,0,0,0,0,0,                                              !/*ca         82*/
     & '55hincorrect syntax:  '//hf)                                            !/*ca         83*/
      go to 10                                                                  !/*ca         84*/
      end                                                                       !/*ca         85*/
