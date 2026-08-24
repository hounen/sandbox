!#ident cprinp.F from deck ci                                                   !/*ci          1*/
      subroutine cprinp                                                         !/*ci          2*/
!        write the cell parameters from the cell cards in scratch file          !/*ci          3*/
!        unit iu1 into cell-parameter cards in scratch file unit iu2.           !/*ci          4*/
      use global_data                                                           !/*cix         1*/
      use imcn_data                                                             !/*cix         2*/
!                                                                               !/*cix         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*cix         4*/
      implicit integer(kindi) (i-n)                                             !/*cix         5*/
!                                                                               !/*cix         6*/
      character hb*80,hc*10,hl*80,hp*10,hq*30,ht*5                              !/*ci4a        1*/
!                                                                               !/*ci          8*/
      endfile iu1                                                               !/*ci          9*/
      rewind iu2                                                                !/*ci         10*/
!                                                                               !/*ci         11*/
!        do all the cell parameters found by celpar.                            !/*ci         12*/
      hl=' '                                                                    !/*ci         13*/
      do 180 ik=1,nkcd                                                          !/*ci         14*/
      if(cnm(ik).eq.'tmp'.and.ncpar(1,ik).ne.0)ncpar(1,ik)=mxt                  !/*ci         15*/
      do 180 mp=1,mipt                                                          !/*ci         16*/
      if(cnm(ik).eq.'wwn'.and.ncpar(mp,ik).ne.0)ncpar(mp,ik)=nww(mp)            !/*ci         17*/
      do 170 in=1,abs(ncpar(mp,ik))                                             !/*ci         18*/
!                                                                               !/*ci         19*/
!        put together the cell-parameter card name.                             !/*ci         20*/
      hc=cnm(ik)                                                                !/*ci         21*/
      i=leng(hc)                                                                !/*ci         22*/
      if(ncpar(mp,ik).lt.0)go to 30                                             !/*ci         23*/
      if(hc.ne.'pd')write(hp,'(i5)')in                                          !/*ci         24*/
      if(hc.eq.'pd')write(hp,'(i5)')ndp(in)                                     !/*ci         25*/
      do 10 j=1,5                                                               !/*ci         26*/
   10 if(hp(j:j).ne.' ')go to 20                                                !/*ci         27*/
   20 hc(i+1:10)=hp(j:5)                                                        !/*ci         28*/
      i=i+6-j                                                                   !/*ci         29*/
   30 if(krq(2,ik).eq.0)go to 40                                                !/*ci         30*/
      hc(i+1:10)=':'//hip(mp:mp)                                                !/*ci         31*/
      i=i+2                                                                     !/*ci         32*/
   40 iw=max(4,i)                                                               !/*ci         33*/
      iv=iw                                                                     !/*ci         34*/
      hb=hc                                                                     !/*ci         35*/
!                                                                               !/*ci         36*/
!        read all the cell cards.                                               !/*ci         37*/
      rewind iu1                                                                !/*ci         38*/
   50 read(iu1,'(a5,a80)',end=170)ht,hl                                         !/*ci4a        2*/
      iu=0                                                                      !/*ci4a        3*/
!                                                                               !/*ci         41*/
!        tentatively provide a 'j' for the next cell.                           !/*ci         42*/
      if(ht(1:1).eq.'&')go to 70                                                !/*ci4a        4*/
      call nxtsym(hl,' ',1,it,iu,1)                                             !/*ci4a        5*/
      iw=iv                                                                     !/*ci         44*/
      ip=0                                                                      !/*ci         45*/
      if(iw+2.le.80)go to 60                                                    !/*ci         46*/
      write(iu2,'(a80)')hb                                                      !/*ci         47*/
      hb=' '                                                                    !/*ci         48*/
      iw=4                                                                      !/*ci         49*/
   60 if(cnm(ik).eq.'imp')go to 70                                              !/*ci4a        6*/
      hb(iw+2:iw+2)='j'                                                         !/*ci4a        7*/
      iv=iw+2                                                                   !/*ci         51*/
!                                                                               !/*ci         52*/
!        look for the cell-parameter card name on this cell card.               !/*ci         53*/
   70 if(ip.lt.0)go to 50                                                       !/*ci         54*/
      if(ip.gt.0)go to 110                                                      !/*ci         55*/
   80 call nxtsym(hl,' =$&',iu+1,it,iu,1)                                       !/*ci4a        8*/
      if(iu.eq.0)go to 50                                                       !/*ci         57*/
      i=min(it+9,iu)                                                            !/*cixk        1*/
      hp=hl(it:i)                                                               !/*cixk        2*/
      jh=0                                                                      !/*cixk        3*/
      if(hp(1:1).eq.'$'.or.hp(1:1).eq.'&')go to 50                              !/*ci4a        9*/
      m=0                                                                       !/*ci         60*/
      do 90 i=1,min(leng(hp),9)                                                 !/*cor4-2    117*/
      if(hp(i:i).eq.':')go to 100                                               !/*ci         62*/
      if(index('0123456789',hp(i:i)).ne.0)go to 90                              !/*ci         63*/
      m=m+1                                                                     !/*ci         64*/
      hq=hp                                                                     !/*ci         65*/
      n=m                                                                       !/*ci         66*/
      if(hp(i:i).eq.'*')n=1                                                     !/*ci         67*/
      if(i.gt.n)hp(n:10)=hq(i:i)//hq(n:i-1)//hq(i+1:10)                         !/*ci         68*/
   90 continue                                                                  !/*ci         69*/
      go to 102                                                                 !/*ci4a       10*/
  100 if(hp(1:i+1).eq.hc)go to 104                                              !/*ci4a       11*/
      if(i.ge.8)go to 102                                                       !/*ci4a       12*/
      if(jh.eq.0)jh=it+i+1                                                      !/*cixk        4*/
      if(jh.ge.iu)go to 102                                                     !/*cixk        5*/
      if(hl(jh:jh).ne.',')go to 102                                             !/*cixk        6*/
      hq=hp(1:i)//hl(jh+1:jh+1)                                                 !/*cixk        7*/
      jh=jh+2                                                                   !/*cixk        8*/
      hp=hq                                                                     !/*ci4a       15*/
      go to 100                                                                 !/*ci4a       16*/
  102 if(hp.eq.hc.or.hp.eq.'*'//hc)go to 104                                    !/*ci4a       17*/
      if(hp.eq.'wwn:'//hip(mp:mp).and.cnm(ik).eq.'wwn')go to 104                !/*ci4a       18*/
      if(hp.ne.'tmp'.or.cnm(ik).ne.'tmp')go to 80                               !/*ci4a       19*/
  104 ip=1                                                                      !/*ci4a       20*/
      nn=-100000                                                                !/*ci         74*/
      mm=0                                                                      !/*ci         75*/
!                                                                               !/*ci         76*/
!        get the next cell parameter value from the cell card.                  !/*ci         77*/
  110 call nxtsym(hl,' =()$&',iu+1,it,iu,1)                                     !/*ci4a       21*/
      if(iu.eq.0)go to 50                                                       !/*ci         79*/
      if(hl(it:it).eq.'$'.or.hl(it:it).eq.'&')go to 50                          !/*ci4a       22*/
      if(hl(it:iu).eq.'=')go to 110                                             !/*ci         81*/
      do 120 i=it,iu-1                                                          !/*ci         82*/
  120 if(index('abcfghijklmnopqrstuvwxyz',hl(i:i)).ne.0)go to 160               !/*ci         83*/
      if(index('abcdefghklnopqstuvwxyz',hl(iu:iu)).ne.0)go to 160               !/*ci         84*/
      hq=hl(it:iu)                                                              !/*ci         85*/
      nn=nn+1                                                                   !/*ci         86*/
      if(hq.eq.'(')nn=0                                                         !/*ci         87*/
      if(hq.eq.')')nn=-100000                                                   !/*ci         88*/
!                                                                               !/*ci         89*/
!        reject wrong entries from tmp or wwn:i list.                           !/*ci         90*/
      if(hp.ne.'tmp'.and.hp.ne.'wwn:'//hip(mp:mp))go to 130                     !/*ci         91*/
      mm=mm+1                                                                   !/*ci         92*/
      if(mm.lt.in)go to 110                                                     !/*ci         93*/
      if(mm.gt.in)go to 160                                                     !/*ci         94*/
!                                                                               !/*ci         95*/
!        take cosine if the entry is an angle in degrees.                       !/*ci         96*/
  130 if(hp.ne.'*fill'.and.hp.ne.'*trcl'.or.nn.lt.4.or.nn.gt.12)                !/*ci         97*/
     & go to 140                                                                !/*ci         98*/
      read(hq,'(bn,e30.0)')a                                                    !/*ci         99*/
      write(hq,'(e20.14)')cos(a*pie/180.)                                       !/*ci        100*/
!                                                                               !/*ci        101*/
!        put the value on the cell parameter card.                              !/*ci        102*/
  140 l=leng(hq)                                                                !/*ci        103*/
      if(iw+l+1.le.80)go to 150                                                 !/*ci        104*/
      if(iv.gt.iw)hb(iv:iv)=' '                                                 !/*ci        105*/
      write(iu2,'(a80)')hb                                                      !/*ci        106*/
      hb=' '                                                                    !/*ci        107*/
      iw=4                                                                      !/*ci        108*/
  150 hb(iw+2:80)=hq                                                            !/*ci        109*/
      iw=iw+l+1                                                                 !/*ci        110*/
      iv=iw                                                                     !/*ci        111*/
      go to 110                                                                 !/*ci        112*/
!                                                                               !/*ci        113*/
!        go read the next cell card.                                            !/*ci        114*/
  160 ip=-1                                                                     !/*ci        115*/
      go to 50                                                                  !/*ci        116*/
  170 write(iu2,'(a80)')hb                                                      !/*ci        117*/
  180 continue                                                                  !/*ci        118*/
!                                                                               !/*ci        119*/
!        get the scratch file ready for reading.                                !/*ci        120*/
      endfile iu2                                                               !/*ci        121*/
      rewind iu2                                                                !/*ci        122*/
      jui=iu2                                                                   !/*ci        123*/
      return                                                                    !/*ci        124*/
      end                                                                       !/*ci        125*/
