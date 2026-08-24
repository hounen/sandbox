!#ident likebt.F from deck li                                                   !/*li          1*/
      subroutine likebt(mm)                                                     !/*li          2*/
!        expand a cell card of the form "n like m but exceptions" into a        !/*li          3*/
!        regular cell card.  mm=1 for call from nxtit1 or nextit.               !/*li          4*/
!        mm=2 for call from oldcd1 or oldcrd.                                   !/*li          5*/
      use global_data                                                           !/*lix         1*/
      use imcn_data                                                             !/*lix         2*/
!                                                                               !/*lix         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*lix         4*/
      implicit integer(kindi) (i-n)                                             !/*lix         5*/
!                                                                               !/*lix         6*/
      save hl1,hl2,ht1,ipp,itl,mats,rhos                                        !/*li4a        1*/
      character hh*30,hl1*80,hl2*80,ht1*5                                       !/*li4a        2*/
!                                                                               !/*li         10*/
      if(mm.eq.2)go to 150                                                      !/*li         11*/
!                                                                               !/*li         12*/
!        when "like" is seen, remove the card from iu1.                         !/*li         13*/
      if(nwc.eq.3)return                                                        !/*li         14*/
      if(nwc.ne.1)go to 10                                                      !/*li         15*/
      likef=1                                                                   !/*li         16*/
      mxa=mxa-1                                                                 !/*li         17*/
      backspace iu1                                                             !/*li         18*/
      call nxtsym(klin,' ',1,it,iu,1)                                           !/*li4a        3*/
      if(klin(it:iu).eq.'like')backspace iu1                                    !/*li4a        4*/
      endfile iu1                                                               !/*li         20*/
      return                                                                    !/*li         21*/
!                                                                               !/*li         22*/
!        get the material and density and copy the geometry of cell "m".        !/*li         23*/
   10 if(nwc.ne.2)go to 120                                                     !/*li         24*/
      do 20 nc=1,mxa                                                            !/*li         25*/
   20 if(mod(ncl(lncl+nc),100000).eq.iitm)go to 30                              !/*li         26*/
      go to 110                                                                 !/*li         27*/
   30 ncl(lncl+mxa+1)=ncl(lncl+mxa+1)+100000*(ncl(lncl+nc)/100000)              !/*li         28*/
      rewind iu2                                                                !/*li         29*/
      rewind iu1                                                                !/*li         30*/
      do 50 i=1,nc                                                              !/*li         31*/
   40 read(iu1,'(a5,a80)')ht1,hl1                                               !/*li4a        5*/
   50 if(ht1(1:1).eq.'&')go to 40                                               !/*li4a        6*/
      ipp=4                                                                     !/*li         34*/
      nw=0                                                                      !/*li         35*/
      hl2=' '                                                                   !/*li         36*/
      call nxtsym(hl1,' ',1,it,iu,1)                                            !/*li4a        7*/
      go to 80                                                                  !/*li4a        8*/
   60 read(iu1,'(a5,a80)',end=110)ht1,hl1                                       !/*li4a        9*/
      if(ht1(1:1).ne.'&')go to 100                                              !/*li4a       10*/
      iu=0                                                                      !/*li4a       11*/
   80 call nxtsym(hl1,' ():#$&',iu+1,itl,iu,1)                                  !/*li4a       12*/
      if(iu.eq.0)go to 60                                                       !/*li         42*/
      if(hl1(itl:itl).eq.'$'.or.hl1(itl:itl).eq.'&')go to 60                    !/*li4a       13*/
      nw=nw+1                                                                   !/*li         43*/
      hh=hl1(itl:iu)                                                            !/*li         44*/
      if(nw.eq.1)read(hh,'(bn,i30)')mats                                        !/*li         45*/
      if(nw.eq.2.and.mats.ne.0)read(hh,'(bn,e30.0)')rhos                        !/*li         46*/
      if(nw.eq.1.or.nw.eq.2.and.mats.ne.0)go to 80                              !/*li         47*/
      k=index('():#',hh(1:1))                                                   !/*li         48*/
      ki=kdata(hh)                                                              !/*li         49*/
      if(k.eq.0.and.ki.eq.0)return                                              !/*li         50*/
      if(k.eq.4)ncomp=ncomp+1                                                   !/*li         51*/
      if(ipp+2+iu-itl.le.80)go to 90                                            !/*li         52*/
      write(iu2,'(a80)')hl2                                                     !/*li         53*/
      ipp=4                                                                     !/*li         54*/
      hl2=' '                                                                   !/*li         55*/
   90 hl2(ipp+2:ipp+2+iu-itl)=hh                                                !/*li         56*/
      ipp=ipp+2+iu-itl                                                          !/*li         57*/
      go to 80                                                                  !/*li         58*/
  100 itl=-1                                                                    !/*li         59*/
      return                                                                    !/*li         60*/
  110 itl=-2                                                                    !/*li         61*/
      return                                                                    !/*li         62*/
!                                                                               !/*li         63*/
!        copy the exceptions onto iu2.                                          !/*li         64*/
  120 if(hitm.eq.'=')return                                                     !/*li         65*/
      if(likef.ne.1)go to 140                                                   !/*li         66*/
      if(hitm.eq.'mat')likef=2                                                  !/*li         67*/
      if(hitm.eq.'rho')likef=3                                                  !/*li         68*/
      if(likef.ne.1)return                                                      !/*li         69*/
      n=0                                                                       !/*li         70*/
      if(hitm.ne.':'.and.hl2(ipp:ipp).ne.':')n=1                                !/*li         71*/
      if(ipp+3+nitm.le.60.or.n.eq.0)go to 130                                   !/*li         72*/
      write(iu2,'(a80)')hl2                                                     !/*li         73*/
      ipp=4                                                                     !/*li         74*/
      hl2=' '                                                                   !/*li         75*/
  130 hl2(ipp+n+1:ipp+n+nitm)=hitm                                              !/*li         76*/
      ipp=ipp+n+nitm                                                            !/*li         77*/
      return                                                                    !/*li         78*/
!                                                                               !/*li         79*/
!        save mat or rho values.                                                !/*li         80*/
  140 if(likef.eq.2)mats=iitm                                                   !/*li         81*/
      if(likef.eq.3)rhos=ritm                                                   !/*li         82*/
      likef=1                                                                   !/*li         83*/
      return                                                                    !/*li         84*/
!                                                                               !/*li         85*/
!        when the "like" card is all read, finish the expanded card.            !/*li         86*/
  150 if(nwc.lt.2)go to 210                                                     !/*li         87*/
      backspace iui                                                             !/*li         88*/
!                                                                               !/*li         89*/
!        copy any cell parameters of cell "m".                                  !/*li         90*/
      if(itl.eq.-2)go to 200                                                    !/*li         91*/
      if(itl.eq.-1)go to 190                                                    !/*li         92*/
      iu=itl-1                                                                  !/*li         93*/
      go to 170                                                                 !/*li         94*/
  160 read(iu1,'(a5,a80)',end=200)ht1,hl1                                       !/*li4a       14*/
      if(ht1(1:1).ne.'&')go to 190                                              !/*li4a       15*/
      iu=0                                                                      !/*li4a       16*/
  170 call nxtsym(hl1,' ()#$&',iu+1,itl,iu,1)                                   !/*li4a       17*/
      if(iu.eq.0)go to 160                                                      !/*li         99*/
      if(hl1(itl:itl).eq.'$')go to 160                                          !/*cor4-2    116*/
      if(ipp+4+iu-itl.le.80)go to 180                                           !/*li        100*/
      write(iu2,'(a80)')hl2                                                     !/*li        101*/
      ipp=4                                                                     !/*li        102*/
      hl2=' '                                                                   !/*li        103*/
  180 hl2(ipp+2:ipp+2+iu-itl)=hl1(itl:iu)                                       !/*li        104*/
      ipp=ipp+2+iu-itl                                                          !/*li        105*/
      go to 170                                                                 !/*li        106*/
!                                                                               !/*li        107*/
!        skip over the rest of iu1.                                             !/*li        108*/
  190 read(iu1,'(a5,a80)',end=200)ht1,hl1                                       !/*li4a       18*/
      go to 190                                                                 !/*li        110*/
  200 backspace iu1                                                             !/*li        111*/
!                                                                               !/*li        112*/
!        get ready to read the expanded card from iu2.                          !/*li        113*/
      write(iu2,'(a80)')hl2                                                     !/*li        114*/
      endfile iu2                                                               !/*li        115*/
      rewind iu2                                                                !/*li        116*/
      jui=iu2                                                                   !/*li        117*/
      write(hlin,'(i5,i6)')icn,mats                                             !/*li        118*/
      if(mats.ne.0)write(hlin(13:33),'(e21.14)')rhos                            !/*li        119*/
  210 likef=0                                                                   !/*li        120*/
      return                                                                    !/*li        121*/
      end                                                                       !/*li        122*/
