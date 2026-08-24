!#ident nsourc.F from deck ns                                                   !/*ns          1*/
      subroutine nsourc(mm)                                                     !/*ns          2*/
!     mm=1, do checking for source cell path memory needs, <[] format           !/*ns          3*/
!     mm=2, split cell paths, when index varies, and reverse path order         !/*ns          4*/
!       with mm=2 a cell path is copied to mq: first loop, the location         !/*ns          5*/
!       of the first index variable is put in jz(1,1), the location             !/*ns          6*/
!       of the last index of this variable is put in jz(1,2), and the           !/*ns          7*/
!       number of unique indices is in jz(1,3), jz(1,4) is the level            !/*ns          8*/
!       number counting from the bottom, and jz(1,5) is zero or points          !/*ns          9*/
!       to the next nz for varying lattice indices.  jz(1,6) is used in         !/*ns         10*/
!       the third loop as a temporary value of this index for a unique          !/*ns         11*/
!       path.  in the first loop this is continued for jz(2,1) to               !/*ns         12*/
!       jz(2,6) for the second index variable, etc.  up to 12                   !/*ns         13*/
!       varying indices are allowed, where nz is the number found.              !/*ns         14*/
!       the first loop also does error checking.  The second loop               !/*ns         15*/
!       reverses the path order if there are no indices that vary. The          !/*ns         16*/
!       third loop splits out individual paths from the indices that            !/*ns         17*/
!       vary [left to right] and reorders to the :() format and moves           !/*ns         18*/
!       things in memory as necessary.                                          !/*ns         19*/
!                                                                               !/*ns         20*/
      use allocate_memory                                                       !/*ns         21*/
      use global_data                                                           !/*ns         22*/
      use imcn_data                                                             !/*ns         23*/
!                                                                               !/*ns         24*/
      implicit real(kindr) (a-h,o-z)                                            !/*ns         25*/
      implicit integer(kindi) (i-n)                                             !/*ns         26*/
!                                                                               !/*ns         27*/
      dimension jz(12,6),mq(300),nv(20,2)                                       !/*ns         28*/
      character ha*8,hh*80                                                      !/*ns         29*/
!                                                                               !/*ns         30*/
      go to (10,170)mm                                                          !/*ns         31*/
!        temporarily store path in mq and then determine number splits          !/*ns         32*/
!        for path and set memory needs.                                         !/*ns         33*/
   10 if(kitm.ne.0)mq(nwc-m6c)=iitm                                             !/*ns         34*/
      if(kitm.ne.0)return                                                       !/*ns         35*/
      if(hitm.eq.'<')mq(nwc-m6c)=1000009                                        !/*ns         36*/
      if(hitm.eq.'[')mq(nwc-m6c)=1000005                                        !/*ns         37*/
      if(hitm.eq.']'.and.mq(nwc-m6c).eq.-999.and.mq(nwc-m6c-2).eq.              !/*ns         38*/
     & 1000005)nwc=nwc+1                                                        !/*ns         39*/
      if(hitm.eq.']')mq(nwc-m6c)=1000006                                        !/*ns         40*/
      if(hitm.eq.':')mq(nwc-m6c)=1000003                                        !/*ns         41*/
      if(hitm.ne.'u'.and.hitm.ne.'=')go to 20                                   !/*ns         42*/
      if(hitm.eq.'='.and.mq(nwc-m6c+1).ne.-999)go to 160                        !/*ns         43*/
      if(hitm.eq.'=')nwc=nwc-1                                                  !/*ns         44*/
      if(hitm.eq.'=')return                                                     !/*ns         45*/
      mq(nwc-m6c+1)=-999                                                        !/*ns         46*/
      nwc=nwc-1                                                                 !/*ns         47*/
      return                                                                    !/*ns         48*/
   20 if(hitm.ne.')')return                                                     !/*ns         49*/
!        determine number splits for path.                                      !/*ns         50*/
      jt=0                                                                      !/*ns         51*/
      kk=0                                                                      !/*ns         52*/
      js=0                                                                      !/*ns         53*/
      jc=2                                                                      !/*ns         54*/
   30 kk=kk+1                                                                   !/*ns         55*/
      if(kk.ge.nwc-m6c)go to 150                                                !/*ns         56*/
      if(mq(kk).gt.1000000)jc=jc+1                                              !/*ns         57*/
      if(mq(kk).eq.1000009.and.js.ne.0.and.jt.eq.0)jt=1                         !/*ns         58*/
      if(mq(kk).eq.1000009.and.js.ne.0)jt=jt*js                                 !/*ns         59*/
      if(mq(kk).eq.1000009)js=0                                                 !/*ns         60*/
      if(mq(kk).eq.1000009)go to 30                                             !/*ns         61*/
      if(mq(kk).eq.1000005)go to 40                                             !/*ns         62*/
      js=js+1                                                                   !/*ns         63*/
      go to 30                                                                  !/*ns         64*/
!        determine number splits jr for lattice.                                !/*ns         65*/
   40 kk=kk+1                                                                   !/*ns         66*/
      jr=0                                                                      !/*ns         67*/
      if(mq(kk).gt.1000000)go to 160                                            !/*ns         68*/
      if(kk+2.ge.nwc-m6c)go to 160                                              !/*ns         69*/
      if(mq(kk+1).eq.-999.and.mq(kk+2).eq.1000006)go to 110                     !/*ns         70*/
      kk=kk+1                                                                   !/*ns         71*/
      if(mq(kk).ne.1000003)go to 60                                             !/*ns         72*/
      if(mq(kk+1)-mq(kk-1).le.0)go to 50                                        !/*ns         73*/
      jr=mq(kk+1)-mq(kk-1)+1                                                    !/*ns         74*/
   50 kk=kk+2                                                                   !/*ns         75*/
   60 if(kk+2.ge.nwc-m6c)go to 160                                              !/*ns         76*/
      kk=kk+1                                                                   !/*ns         77*/
      if(mq(kk).ne.1000003)go to 80                                             !/*ns         78*/
      if(mq(kk+1)-mq(kk-1).le.0)go to 70                                        !/*ns         79*/
      if(jr.ne.0)jr=jr*(mq(kk+1)-mq(kk-1)+1)                                    !/*ns         80*/
      if(jr.eq.0)jr=mq(kk+1)-mq(kk-1)+1                                         !/*ns         81*/
   70 kk=kk+2                                                                   !/*ns         82*/
   80 if(kk+1.ge.nwc-m6c)go to 160                                              !/*ns         83*/
      kk=kk+1                                                                   !/*ns         84*/
      if(mq(kk).eq.1000006)go to 100                                            !/*ns         85*/
      if(mq(kk).ne.1000003)go to 160                                            !/*ns         86*/
      if(mq(kk+1)-mq(kk-1).le.0)go to 90                                        !/*ns         87*/
      if(jr.ne.0)jr=jr*(mq(kk+1)-mq(kk-1)+1)                                    !/*ns         88*/
      if(jr.eq.0)jr=mq(kk+1)-mq(kk-1)+1                                         !/*ns         89*/
   90 kk=kk+2                                                                   !/*ns         90*/
      if(mq(kk).ne.1000006)go to 160                                            !/*ns         91*/
  100 if(jr.ne.0)js=js+jr-1                                                     !/*ns         92*/
      go to 30                                                                  !/*ns         93*/
!        special u= lattice.                                                    !/*ns         94*/
  110 do 120 ic=1,mxa                                                           !/*ns         95*/
      j9=mod(ncl(lncl+ic),100000)                                               !/*ns         96*/
  120 if(abs(j9).eq.abs(mq(kk-2)))go to 130                                     !/*ns         97*/
      go to 160                                                                 !/*ns         98*/
!        above is needed because namchg does not yet work during                !/*ns         99*/
!        first pass, even though the fill matrix is available.                  !/*ns        100*/
  130 if(mfl(lmfl+1,ic).ge.0)call erprnt(1,1,0,0,0,0,0,0,                       !/*ns        101*/
     & '59hlattice universe of cell path on si/ds card does not exist.')        !/*ns        102*/
      if(mfl(lmfl+1,ic).ge.0)go to 160                                          !/*ns        103*/
      lp=-mfl(lmfl+1,ic)                                                        !/*ns        104*/
      do 140 j=3,2+laf(lp+1,2)*laf(lp+2,2)*laf(lp+3,2)                          !/*ns        105*/
  140 if(laf(lp+1,j).eq.abs(mq(kk)))jr=jr+1                                     !/*ns        106*/
      if(jr.le.1)go to 160                                                      !/*ns        107*/
      kk=kk+2                                                                   !/*ns        108*/
      if(mq(kk).ne.1000006)go to 160                                            !/*ns        109*/
      if(jr.ne.0)js=js+jr-1                                                     !/*ns        110*/
      go to 30                                                                  !/*ns        111*/
  150 if(js.ne.0.and.jt.eq.0)jt=1                                               !/*ns        112*/
      if(js.ne.0)jt=jt*js                                                       !/*ns        113*/
      m5c=0                                                                     !/*ns        114*/
      m6c=0                                                                     !/*ns        115*/
      if(jt.le.1)return                                                         !/*ns        116*/
!        increment memory to account for path splits.                           !/*ns        117*/
      mxxs=mxxs+4*(nwc-m6c+1-jc)*(jt-1)                                         !/*ns        118*/
      mkcp=mkcp+4*jc*(jt-1)                                                     !/*ns        119*/
      return                                                                    !/*ns        120*/
  160 call erprnt(1,1,0,0,0,0,0,0,                                              !/*ns        121*/
     & '51hthe format of a cell path on si/ds card incorrect .')                !/*ns        122*/
      m5c=0                                                                     !/*ns        123*/
      return                                                                    !/*ns        124*/
!        begin examination of path with <[] format.                             !/*ns        125*/
  170 ii=lkcp+mkcp                                                              !/*ns        126*/
      jp=0                                                                      !/*ns        127*/
      i=ii                                                                      !/*ns        128*/
  180 i=i-1                                                                     !/*ns        129*/
      if(kcp(i).ne.1000007)go to 180                                            !/*ns        130*/
!        put <[] format path in mq, beginning first loop.                       !/*ns        131*/
      do 190 kj=i+1,ii-1                                                        !/*ns        132*/
  190 mq(kj-i)=kcp(kj)                                                          !/*ns        133*/
      nz=0                                                                      !/*ns        134*/
      mz=0                                                                      !/*ns        135*/
      do 200 j2=1,12                                                            !/*ns        136*/
      do 200 j3=1,6                                                             !/*ns        137*/
  200 jz(j2,j3)=0                                                               !/*ns        138*/
      j2=1                                                                      !/*ns        139*/
      j3=0                                                                      !/*ns        140*/
!        js=number permuations for a level, jt=grand total.                     !/*ns        141*/
      jt=0                                                                      !/*ns        142*/
!        check for errors and set index locators within path.                   !/*ns        143*/
      if(ii-i-1.le.2.or.mq(3).gt.1000000)                                       !/*ns        144*/
     & call erprnt(1,1,0,0,0,0,0,0,                                             !/*ns        145*/
     & '39hno cell on si/ds card for lowest level.')                            !/*ns        146*/
      if(ii-i-1.le.2.or.mq(3).gt.1000000)go to 890                              !/*ns        147*/
      nv(j2,1)=3                                                                !/*ns        148*/
      nv(j2,2)=0                                                                !/*ns        149*/
      if(ii-i-1.eq.3)go to 400                                                  !/*ns        150*/
      if(mq(4).ne.1000009)call erprnt(1,1,0,0,0,0,0,0,                          !/*ns        151*/
     & '53honly one cell allowed on si/ds card for lowest level.')              !/*ns        152*/
      if(mq(4).ne.1000009)go to 890                                             !/*ns        153*/
      if(ii-i-1.eq.4)call erprnt(1,1,0,0,0,0,0,0,                               !/*ns        154*/
     & '52hno cell specified in path on si/ds card for a level.')               !/*ns        155*/
      if(ii-i-1.eq.4)go to 890                                                  !/*ns        156*/
      if(mq(ii-i-1).gt.1000000.and.mq(ii-i-1).ne.1000006)                       !/*ns        157*/
     & call erprnt(1,1,0,0,0,0,0,0,                                             !/*ns        158*/
     & '58hno cell specified in path on si/ds card for highest level.')         !/*ns        159*/
      if(mq(ii-i-1).gt.1000000.and.mq(ii-i-1).ne.1000006)go to 890              !/*ns        160*/
      mz=5                                                                      !/*ns        161*/
      kk=5                                                                      !/*ns        162*/
      ny=nz+1                                                                   !/*ns        163*/
      j2=2                                                                      !/*ns        164*/
      nv(j2,1)=kk                                                               !/*ns        165*/
      nv(j2,2)=0                                                                !/*ns        166*/
      js=0                                                                      !/*ns        167*/
  210 if(mz.eq.kk.and.mq(kk).eq.mq(kk-1))call erprnt(1,1,0,0,0,0,0,0,           !/*ns        168*/
     & '52hno cell specified in path on si/ds card for a level.')               !/*ns        169*/
      if(mz.eq.kk.and.mq(kk).eq.mq(kk-1))go to 890                              !/*ns        170*/
      if(mq(kk).ne.1000009)go to 220                                            !/*ns        171*/
      if(js.ne.0.and.jt.eq.0)jt=1                                               !/*ns        172*/
      if(js.ne.0)jt=jt*js                                                       !/*ns        173*/
      js=0                                                                      !/*ns        174*/
      j2=j2+1                                                                   !/*ns        175*/
      nv(j2,1)=kk+1                                                             !/*ns        176*/
      nv(j2,2)=0                                                                !/*ns        177*/
      go to 380                                                                 !/*ns        178*/
  220 if(mq(kk).eq.1000005)go to 240                                            !/*ns        179*/
      if(mq(kk).gt.1000000)call erprnt(1,1,0,0,0,0,0,0,                         !/*ns        180*/
     & '50hthe format of a cell path on si/ds card incorrect.')                 !/*ns        181*/
      if(mq(kk).gt.1000000)go to 890                                            !/*ns        182*/
      if(mz.eq.kk)go to 390                                                     !/*ns        183*/
      if(mz.eq.0)go to 230                                                      !/*ns        184*/
      if(nz.eq.12)call erprnt(1,1,0,0,0,0,0,0,                                  !/*ns        185*/
     & '54hmaximum number indices in path on si/ds card exceeded.')             !/*ns        186*/
      if(nz.eq.12)go to 890                                                     !/*ns        187*/
      if(j3.ne.j2)j3=j2                                                         !/*ns        188*/
      nz=nz+1                                                                   !/*ns        189*/
      jz(nz,1)=mz                                                               !/*ns        190*/
      jz(nz,2)=kk                                                               !/*ns        191*/
      jz(nz,3)=2                                                                !/*ns        192*/
      jz(nz,4)=j2                                                               !/*ns        193*/
      jz(nz,5)=0                                                                !/*ns        194*/
      js=js+2                                                                   !/*ns        195*/
      mz=0                                                                      !/*ns        196*/
      nv(j2,2)=nz                                                               !/*ns        197*/
      go to 390                                                                 !/*ns        198*/
  230 jz(ny,2)=kk                                                               !/*ns        199*/
      jz(ny,3)=jz(ny,3)+1                                                       !/*ns        200*/
      js=js+1                                                                   !/*ns        201*/
      go to 390                                                                 !/*ns        202*/
!        deal with a lattice.                                                   !/*ns        203*/
  240 if(mq(kk-1).gt.1000000.or.mq(kk-1).eq.0)                                  !/*ns        204*/
     & call erprnt(1,1,0,0,0,0,0,0,                                             !/*ns        205*/
     & '52hno cell specified in path on si/ds card for a level.')               !/*ns        206*/
      if(mq(kk-1).gt.1000000.or.mq(kk-1).eq.0)go to 890                         !/*ns        207*/
      ks=kk-1                                                                   !/*ns        208*/
      kk=kk+1                                                                   !/*ns        209*/
      jr=0                                                                      !/*ns        210*/
      jf=0                                                                      !/*ns        211*/
      if(kk+1.ge.ii-i)call erprnt(1,1,0,0,0,0,0,0,                              !/*ns        212*/
     & '38hcell path on si/ds card is incomplete.')                             !/*ns        213*/
      if(kk+1.ge.ii-i)go to 890                                                 !/*ns        214*/
      if(mq(kk).gt.1000000)call erprnt(1,1,0,0,0,0,0,0,                         !/*ns        215*/
     & '50hthe format of a cell path on si/ds card incorrect.')                 !/*ns        216*/
      if(mq(kk).gt.1000000)go to 890                                            !/*ns        217*/
      if(mq(kk+1).eq.1000006.and.mq(kk).le.0)                                   !/*ns        218*/
     & call erprnt(1,1,0,0,0,0,0,0,                                             !/*ns        219*/
     & '53hlattice index   of cell path on si/ds card incorrect.')              !/*ns        220*/
      if(mq(kk+1).eq.1000006.and.mq(kk).le.0)go to 890                          !/*ns        221*/
      if(mq(kk+1).eq.1000006)kk=kk+1                                            !/*ns        222*/
      if(mq(kk).eq.1000006)go to 390                                            !/*ns        223*/
      if(mq(kk+1).eq.-999.and.mq(kk+2).eq.1000006)go to 350                     !/*ns        224*/
      if(kk+3.gt.ii-i-1)call erprnt(1,1,0,0,0,0,0,0,                            !/*ns        225*/
     & '38hcell path on si/ds card is incomplete.')                             !/*ns        226*/
      if(kk+3.gt.ii-i-1)go to 890                                               !/*ns        227*/
      kk=kk+1                                                                   !/*ns        228*/
      if(mq(kk).lt.1000000)go to 270                                            !/*ns        229*/
      if(mq(kk).ne.1000003.or.mq(kk-1).gt.mq(kk+1))                             !/*ns        230*/
     & call erprnt(1,1,0,0,0,0,0,0,                                             !/*ns        231*/
     & '53hlattice indices of cell path on si/ds card incorrect.')              !/*ns        232*/
      if(mq(kk).ne.1000003.or.mq(kk-1).gt.mq(kk+1))go to 890                    !/*ns        233*/
      if(mq(kk+1)-mq(kk-1).le.0)go to 260                                       !/*ns        234*/
      if(nz+1.gt.ny)go to 250                                                   !/*ns        235*/
      if(nz.eq.12)call erprnt(1,1,0,0,0,0,0,0,                                  !/*ns        236*/
     & '54hmaximum number indices in path on si/ds card exceeded.')             !/*ns        237*/
      if(nz.eq.12)go to 890                                                     !/*ns        238*/
      nz=nz+1                                                                   !/*ns        239*/
      jz(nz,1)=mz                                                               !/*ns        240*/
      jz(nz,2)=mz                                                               !/*ns        241*/
      jz(nz,3)=1                                                                !/*ns        242*/
      jz(nz,4)=j2                                                               !/*ns        243*/
      nv(j2,2)=nz                                                               !/*ns        244*/
      js=js+1                                                                   !/*ns        245*/
  250 if(nz.eq.12)call erprnt(1,1,0,0,0,0,0,0,                                  !/*ns        246*/
     & '54hmaximum number indices in path on si/ds card exceeded.')             !/*ns        247*/
      if(nz.eq.12)go to 890                                                     !/*ns        248*/
      nz=nz+1                                                                   !/*ns        249*/
      jz(nz,1)=kk-1                                                             !/*ns        250*/
      jz(nz,2)=ks                                                               !/*ns        251*/
      jz(nz,3)=mq(kk+1)-mq(kk-1)+1                                              !/*ns        252*/
      jz(nz,4)=j2                                                               !/*ns        253*/
      jz(nz,5)=0                                                                !/*ns        254*/
      jr=mq(kk+1)-mq(kk-1)+1                                                    !/*ns        255*/
      jf=1                                                                      !/*ns        256*/
  260 kk=kk+2                                                                   !/*ns        257*/
  270 if(kk+2.gt.ii-i-1)call erprnt(1,1,0,0,0,0,0,0,                            !/*ns        258*/
     & '38hcell path on si/ds card is incomplete.')                             !/*ns        259*/
      if(kk+2.gt.ii-i-1)go to 890                                               !/*ns        260*/
      kk=kk+1                                                                   !/*ns        261*/
      if(mq(kk).lt.1000000)go to 300                                            !/*ns        262*/
      if(mq(kk).ne.1000003.or.mq(kk-1).gt.mq(kk+1))                             !/*ns        263*/
     & call erprnt(1,1,0,0,0,0,0,0,                                             !/*ns        264*/
     & '53hlattice indices of cell path on si/ds card incorrect.')              !/*ns        265*/
      if(mq(kk).ne.1000003.or.mq(kk-1).gt.mq(kk+1))go to 890                    !/*ns        266*/
      if(mq(kk+1)-mq(kk-1).le.0)go to 290                                       !/*ns        267*/
      if(nz+1.gt.ny)go to 280                                                   !/*ns        268*/
      if(nz.eq.12)call erprnt(1,1,0,0,0,0,0,0,                                  !/*ns        269*/
     & '54hmaximum number indices in path on si/ds card exceeded.')             !/*ns        270*/
      if(nz.eq.12)go to 890                                                     !/*ns        271*/
      nz=nz+1                                                                   !/*ns        272*/
      jz(nz,1)=mz                                                               !/*ns        273*/
      jz(nz,2)=mz                                                               !/*ns        274*/
      jz(nz,3)=1                                                                !/*ns        275*/
      jz(nz,4)=j2                                                               !/*ns        276*/
      nv(j2,2)=nz                                                               !/*ns        277*/
      js=js+1                                                                   !/*ns        278*/
  280 if(nz.eq.12)call erprnt(1,1,0,0,0,0,0,0,                                  !/*ns        279*/
     & '54hmaximum number indices in path on si/ds card exceeded.')             !/*ns        280*/
      if(nz.eq.12)go to 890                                                     !/*ns        281*/
      nz=nz+1                                                                   !/*ns        282*/
      jz(nz,1)=kk-1                                                             !/*ns        283*/
      jz(nz,2)=ks                                                               !/*ns        284*/
      jz(nz,3)=mq(kk+1)-mq(kk-1)+1                                              !/*ns        285*/
      jz(nz,4)=j2                                                               !/*ns        286*/
      jz(nz,5)=0                                                                !/*ns        287*/
      if(jf.eq.1)jz(nz-1,5)=nz                                                  !/*ns        288*/
      if(jr.ne.0)jr=jr*(mq(kk+1)-mq(kk-1)+1)                                    !/*ns        289*/
      if(jr.eq.0)jr=mq(kk+1)-mq(kk-1)+1                                         !/*ns        290*/
      if(jf.eq.0)jf=2                                                           !/*ns        291*/
  290 kk=kk+2                                                                   !/*ns        292*/
  300 if(kk+1.gt.ii-i-1)call erprnt(1,1,0,0,0,0,0,0,                            !/*ns        293*/
     & '38hcell path on si/ds card is incomplete.')                             !/*ns        294*/
      if(kk+1.gt.ii-i-1)go to 890                                               !/*ns        295*/
      kk=kk+1                                                                   !/*ns        296*/
      if(mq(kk).ne.1000003)go to 330                                            !/*ns        297*/
      if(kk+1.gt.ii-i-1)call erprnt(1,1,0,0,0,0,0,0,                            !/*ns        298*/
     & '38hcell path on si/ds card is incomplete.')                             !/*ns        299*/
      if(kk+1.gt.ii-i-1)go to 890                                               !/*ns        300*/
      if(mq(kk+1)-mq(kk-1).le.0)go to 320                                       !/*ns        301*/
      if(nz+1.gt.ny)go to 310                                                   !/*ns        302*/
      if(nz.eq.12)call erprnt(1,1,0,0,0,0,0,0,                                  !/*ns        303*/
     & '54hmaximum number indices in path on si/ds card exceeded.')             !/*ns        304*/
      if(nz.eq.12)go to 890                                                     !/*ns        305*/
      nz=nz+1                                                                   !/*ns        306*/
      jz(nz,1)=mz                                                               !/*ns        307*/
      jz(nz,2)=mz                                                               !/*ns        308*/
      jz(nz,3)=1                                                                !/*ns        309*/
      jz(nz,4)=j2                                                               !/*ns        310*/
      nv(j2,2)=nz                                                               !/*ns        311*/
      js=js+1                                                                   !/*ns        312*/
  310 if(nz.eq.12)call erprnt(1,1,0,0,0,0,0,0,                                  !/*ns        313*/
     & '54hmaximum number indices in path on si/ds card exceeded.')             !/*ns        314*/
      if(nz.eq.12)go to 890                                                     !/*ns        315*/
      nz=nz+1                                                                   !/*ns        316*/
      jz(nz,1)=kk-1                                                             !/*ns        317*/
      jz(nz,2)=ks                                                               !/*ns        318*/
      jz(nz,3)=mq(kk+1)-mq(kk-1)+1                                              !/*ns        319*/
      jz(nz,4)=j2                                                               !/*ns        320*/
      jz(nz,5)=0                                                                !/*ns        321*/
      if(jf.ne.0)jz(nz-1,5)=nz                                                  !/*ns        322*/
      if(jr.ne.0)jr=jr*(mq(kk+1)-mq(kk-1)+1)                                    !/*ns        323*/
      if(jr.eq.0)jr=mq(kk+1)-mq(kk-1)+1                                         !/*ns        324*/
      if(jf.eq.0)jf=3                                                           !/*ns        325*/
  320 kk=kk+2                                                                   !/*ns        326*/
  330 continue                                                                  !/*ns        327*/
      if(jf.eq.0)go to 340                                                      !/*ns        328*/
      mz=0                                                                      !/*ns        329*/
      js=js+jr-1                                                                !/*ns        330*/
      j3=j2                                                                     !/*ns        331*/
  340 if(mq(kk).ne.1000006)call erprnt(1,1,0,0,0,0,0,0,                         !/*ns        332*/
     & '38hcell path on si/ds card is incomplete.')                             !/*ns        333*/
      if(mq(kk).ne.1000006)go to 890                                            !/*ns        334*/
      go to 390                                                                 !/*ns        335*/
!        deal with u= inside lattice indices.                                   !/*ns        336*/
  350 jr=0                                                                      !/*ns        337*/
      ic=namchg(1,abs(mq(kk-2)))                                                !/*ns        338*/
      if(mfl(lmfl+1,ic).ge.0)call erprnt(1,1,0,0,0,0,0,0,                       !/*ns        339*/
     & '59hlattice universe of cell path on si/ds card does not exist.')        !/*ns        340*/
      if(mfl(lmfl+1,ic).ge.0)go to 890                                          !/*ns        341*/
      lp=-mfl(lmfl+1,ic)                                                        !/*ns        342*/
      do 360 j=3,2+laf(lp+1,2)*laf(lp+2,2)*laf(lp+3,2)                          !/*ns        343*/
  360 if(laf(lp+1,j).eq.abs(mq(kk)))jr=jr+1                                     !/*ns        344*/
      if(jr.eq.0)call erprnt(1,1,0,0,0,0,0,0,                                   !/*ns        345*/
     & '59hlattice universe of cell path on si/ds card does not exist.')        !/*ns        346*/
      if(jr.eq.1)call erprnt(1,1,0,0,0,0,0,0,                                   !/*ns        347*/
     & '57honly one element of u= of cell path on si/ds card exists.')          !/*ns        348*/
      if(jr.le.1)go to 890                                                      !/*ns        349*/
      j3=j2                                                                     !/*ns        350*/
      if(nz+1.gt.ny)go to 370                                                   !/*ns        351*/
      if(nz.eq.12)call erprnt(1,1,0,0,0,0,0,0,                                  !/*ns        352*/
     & '54hmaximum number indices in path on si/ds card exceeded.')             !/*ns        353*/
      if(nz.eq.12)go to 890                                                     !/*ns        354*/
      nz=nz+1                                                                   !/*ns        355*/
      jz(nz,1)=mz                                                               !/*ns        356*/
      jz(nz,2)=mz                                                               !/*ns        357*/
      jz(nz,3)=1                                                                !/*ns        358*/
      jz(nz,4)=j2                                                               !/*ns        359*/
      nv(j2,2)=nz                                                               !/*ns        360*/
      js=js+1                                                                   !/*ns        361*/
  370 if(nz.eq.12)call erprnt(1,1,0,0,0,0,0,0,                                  !/*ns        362*/
     & '54hmaximum number indices in path on si/ds card exceeded.')             !/*ns        363*/
      if(nz.eq.12)go to 890                                                     !/*ns        364*/
      nz=nz+1                                                                   !/*ns        365*/
      jz(nz,1)=kk                                                               !/*ns        366*/
      jz(nz,2)=ks                                                               !/*ns        367*/
      jz(nz,3)=jr                                                               !/*ns        368*/
      jz(nz,4)=j2                                                               !/*ns        369*/
      jz(nz,5)=0                                                                !/*ns        370*/
      mz=0                                                                      !/*ns        371*/
      js=js+jr-1                                                                !/*ns        372*/
      kk=kk+2                                                                   !/*ns        373*/
      go to 390                                                                 !/*ns        374*/
  380 mz=kk+1                                                                   !/*ns        375*/
      ny=nz+1                                                                   !/*ns        376*/
  390 kk=kk+1                                                                   !/*ns        377*/
!        return to statement 210 for next item in cell path.                    !/*ns        378*/
      if(kk.le.ii-i-1)go to 210                                                 !/*ns        379*/
      if(js.ne.0.and.jt.eq.0)jt=1                                               !/*ns        380*/
      if(js.ne.0)jt=jt*js                                                       !/*ns        381*/
  400 continue                                                                  !/*ns        382*/
      if(jt.gt.1)go to 520                                                      !/*ns        383*/
!        begin second loop to convert <[] format to :() when path               !/*ns        384*/
!        with <[] format remains as one path with no splits.                    !/*ns        385*/
      kcp(i)=kcp(i+1)                                                           !/*ns        386*/
      kcp(i+1)=kcp(i+2)                                                         !/*ns        387*/
      kj=i+2                                                                    !/*ns        388*/
!        write out one level at a time starting with highest as j               !/*ns        389*/
      do 440 j9=1,j2                                                            !/*ns        390*/
      j=j2-(j9-1)                                                               !/*ns        391*/
      kk=nv(j,1)-1                                                              !/*ns        392*/
  410 kk=kk+1                                                                   !/*ns        393*/
      if(kk.eq.ii-i)go to 440                                                   !/*ns        394*/
      if(mq(kk).eq.1000009)go to 440                                            !/*ns        395*/
      if(mq(kk).eq.1000005)go to 420                                            !/*ns        396*/
      kcp(kj)=mq(kk)                                                            !/*ns        397*/
      kj=kj+1                                                                   !/*ns        398*/
      go to 410                                                                 !/*ns        399*/
!        deal with a lattice element.                                           !/*ns        400*/
  420 kcp(kj)=1000001                                                           !/*ns        401*/
      kj=kj+1                                                                   !/*ns        402*/
      kk=kk+1                                                                   !/*ns        403*/
      if(mq(kk+1).eq.1000006)go to 430                                          !/*ns        404*/
      kcp(kj)=mq(kk)                                                            !/*ns        405*/
      kj=kj+1                                                                   !/*ns        406*/
      if(mq(kk+1).eq.1000003)kk=kk+2                                            !/*ns        407*/
      kk=kk+1                                                                   !/*ns        408*/
      kcp(kj)=mq(kk)                                                            !/*ns        409*/
      kj=kj+1                                                                   !/*ns        410*/
      if(mq(kk+1).eq.1000003)kk=kk+2                                            !/*ns        411*/
      kk=kk+1                                                                   !/*ns        412*/
      kcp(kj)=mq(kk)                                                            !/*ns        413*/
      kj=kj+1                                                                   !/*ns        414*/
      kcp(kj)=1000002                                                           !/*ns        415*/
      kj=kj+1                                                                   !/*ns        416*/
      if(mq(kk+1).eq.1000003)kk=kk+2                                            !/*ns        417*/
      kk=kk+1                                                                   !/*ns        418*/
      go to 410                                                                 !/*ns        419*/
!        lattice element uses only one locator, expand to 3.                    !/*ns        420*/
  430 ic=namchg(1,abs(mq(kk-2)))                                                !/*ns        421*/
      if(mfl(lmfl+1,ic).ge.0)call erprnt(1,1,0,0,0,0,0,0,                       !/*ns        422*/
     & '59hlattice universe of cell path on si/ds card does not exist.')        !/*ns        423*/
      if(mfl(lmfl+1,ic).ge.0)go to 890                                          !/*ns        424*/
      lp=-mfl(lmfl+1,ic)                                                        !/*ns        425*/
      k8=(mq(kk)-1.)/(laf(lp+1,2)*laf(lp+2,2))                                  !/*ns        426*/
      k8=k8+1                                                                   !/*ns        427*/
      if(k8.gt.laf(lp+3,2))call erprnt(1,1,0,0,0,0,0,0,                         !/*ns        428*/
     & '53hlattice index   of cell path on si/ds card incorrect.')              !/*ns        429*/
      if(k8.gt.laf(lp+3,2))go to 890                                            !/*ns        430*/
      j8=(mq(kk)-(k8-1)*laf(lp+1,2)*laf(lp+2,2)-1.)/laf(lp+1,2)                 !/*ns        431*/
      j8=j8+1                                                                   !/*ns        432*/
      i8=mq(kk)-(k8-1)*laf(lp+1,2)*laf(lp+2,2)-(j8-1)*laf(lp+1,2)               !/*ns        433*/
!        switch index to actual rather than beginning with 1                    !/*ns        434*/
      i8=i8+laf(lp+1,1)-1                                                       !/*ns        435*/
      j8=j8+laf(lp+2,1)-1                                                       !/*ns        436*/
      k8=k8+laf(lp+3,1)-1                                                       !/*ns        437*/
      kcp(kj)=i8                                                                !/*ns        438*/
      kj=kj+1                                                                   !/*ns        439*/
      kcp(kj)=j8                                                                !/*ns        440*/
      kj=kj+1                                                                   !/*ns        441*/
      kcp(kj)=k8                                                                !/*ns        442*/
      kj=kj+1                                                                   !/*ns        443*/
      kcp(kj)=1000002                                                           !/*ns        444*/
      kj=kj+1                                                                   !/*ns        445*/
      kk=kk+1                                                                   !/*ns        446*/
      go to 410                                                                 !/*ns        447*/
  440 continue                                                                  !/*ns        448*/
!        set number entries for running code and zero out beyond.               !/*ns        449*/
      kcp(i+1)=kj-i-2                                                           !/*ns        450*/
  450 do 460 ij=kj,ii                                                           !/*ns        451*/
  460 kcp(ij)=0                                                                 !/*ns        452*/
!        set end point to identify <[] format for printing.                     !/*ns        453*/
      kcp(kj)=1000008                                                           !/*ns        454*/
      return                                                                    !/*ns        455*/
!        begin third loop when path with <[] format has a split.                !/*ns        456*/
  520 ix=ii-i+1                                                                 !/*ns        457*/
      is=ii                                                                     !/*ns        458*/
      do 880 jq=1,jt                                                            !/*ns        459*/
      kj=i+(jq-1)*ix                                                            !/*ns        460*/
      if(jq.eq.1)kcp(kj)=kcp(kj+1)                                              !/*ns        461*/
      if(jq.ne.1)kcp(kj)=kcp(kj-ix)                                             !/*ns        462*/
      kj=kj+1                                                                   !/*ns        463*/
      if(jq.eq.1)kcp(kj)=kcp(kj+1)                                              !/*ns        464*/
      if(jq.ne.1)kcp(kj)=kcp(kj-ix)                                             !/*ns        465*/
      kj=kj+1                                                                   !/*ns        466*/
      ju=j2                                                                     !/*ns        467*/
      ms=0                                                                      !/*ns        468*/
!        mx=current level being incremented, ms=temporary save of mx.           !/*ns        469*/
      if(jq.eq.1)go to 550                                                      !/*ns        470*/
      m2c=m2c+1                                                                 !/*ns        471*/
!        following loop taken from ny.157 to ny.165                             !/*ns        472*/
      if(ksd(lksd+20,m1c).eq.0.or.m2c.lt.ksd(lksd+20,m1c))go to 540             !/*ns        473*/
      do 530 iz=m1c+1,msd                                                       !/*ns        474*/
      iy=msd+m1c+1-iz                                                           !/*ns        475*/
      ksd(lksd+13,iy)=ksd(lksd+13,iy)+4                                         !/*ns        476*/
      m=max(ksd(lksd+4,iy),ksd(lksd+20,iy))                                     !/*ns        477*/
      do 530 j=1,m                                                              !/*ns        478*/
      do 530 k=1,4                                                              !/*ns        479*/
  530 spf(ksd(lksd+13,iy)+k,m+1-j)=spf(ksd(lksd+13,iy)+k,m-j)                   !/*ns        480*/
  540 m3c=kj-lkcp                                                               !/*ns        481*/
      spf(ksd(lksd+13,m1c)+1,m2c)=-lkcp-m3c                                     !/*ns        482*/
      go to 610                                                                 !/*ns        483*/
!        initialize indexes of splits that will vary.                           !/*ns        484*/
  550 mx=0                                                                      !/*ns        485*/
      do 600 j9=1,ju                                                            !/*ns        486*/
      if(nv(j9,2).eq.0)go to 600                                                !/*ns        487*/
      if(mx.eq.0)mx=j9                                                          !/*ns        488*/
      mz=nv(j9,2)                                                               !/*ns        489*/
      jz(mz,6)=jz(mz,1)                                                         !/*ns        490*/
      if(mq(jz(mz,1)+1).ne.1000005)go to 600                                    !/*ns        491*/
!        check if a lattice element varies.                                     !/*ns        492*/
      kz=mz                                                                     !/*ns        493*/
  560 kz=kz+1                                                                   !/*ns        494*/
      if(kz.gt.nz)go to 600                                                     !/*ns        495*/
      if(jz(kz,4).ne.j9)go to 600                                               !/*ns        496*/
      if(jz(kz,2).ne.jz(mz,1))go to 560                                         !/*ns        497*/
      kk=jz(kz,1)                                                               !/*ns        498*/
      if(mq(kk+1).eq.-999.and.mq(kk+2).eq.1000006)go to 570                     !/*ns        499*/
!        initialize lattice elements for this cell.                             !/*ns        500*/
      jz(kz,6)=mq(kk)                                                           !/*ns        501*/
      if(jz(kz,5).eq.0)go to 600                                                !/*ns        502*/
      kz=jz(kz,5)                                                               !/*ns        503*/
      jz(kz,6)=mq(jz(kz,1))                                                     !/*ns        504*/
      if(jz(kz,5).eq.0)go to 600                                                !/*ns        505*/
      kz=jz(kz,5)                                                               !/*ns        506*/
      jz(kz,6)=mq(jz(kz,1))                                                     !/*ns        507*/
      go to 600                                                                 !/*ns        508*/
!        special treatment for lattice (u=).                                    !/*ns        509*/
  570 ic=namchg(1,abs(mq(kk-2)))                                                !/*ns        510*/
      lp=-mfl(lmfl+1,ic)                                                        !/*ns        511*/
      do 580 jm=3,2+laf(lp+1,2)*laf(lp+2,2)*laf(lp+3,2)                         !/*ns        512*/
  580 if(laf(lp+1,jm).eq.abs(mq(kk)))go to 590                                  !/*ns        513*/
  590 jz(kz,6)=jm                                                               !/*ns        514*/
  600 continue                                                                  !/*ns        515*/
      if(ms.eq.0)go to 750                                                      !/*ns        516*/
      ks=mx                                                                     !/*ns        517*/
      mx=ms                                                                     !/*ns        518*/
      ms=ks                                                                     !/*ns        519*/
!        advance indices to next split, mx is level being incremented.          !/*ns        520*/
  610 mz=nv(mx,2)                                                               !/*ns        521*/
      kk=jz(mz,6)                                                               !/*ns        522*/
!        check if a lattice element varies.                                     !/*ns        523*/
      kz=mz                                                                     !/*ns        524*/
  620 kz=kz+1                                                                   !/*ns        525*/
      if(kz.gt.nz)go to 680                                                     !/*ns        526*/
      if(jz(kz,4).ne.mx)go to 680                                               !/*ns        527*/
      if(jz(kz,2).ne.kk)go to 620                                               !/*ns        528*/
      kk=jz(kz,1)                                                               !/*ns        529*/
      if(mq(kk+1).eq.-999.and.mq(kk+2).eq.1000006)go to 650                     !/*ns        530*/
!        advance next lattice element.                                          !/*ns        531*/
      if(jz(kz,6).ge.mq(kk+2))go to 630                                         !/*ns        532*/
      jz(kz,6)=jz(kz,6)+1                                                       !/*ns        533*/
      go to 750                                                                 !/*ns        534*/
  630 if(jz(kz,5).eq.0)go to 680                                                !/*ns        535*/
      kz=jz(kz,5)                                                               !/*ns        536*/
      kb=jz(kz,1)                                                               !/*ns        537*/
      if(jz(kz,6).ge.mq(kb+2))go to 640                                         !/*ns        538*/
      jz(kz,6)=jz(kz,6)+1                                                       !/*ns        539*/
      jz(kz-1,6)=mq(kk)                                                         !/*ns        540*/
      go to 750                                                                 !/*ns        541*/
  640 if(jz(kz,5).eq.0)go to 680                                                !/*ns        542*/
      kz=jz(kz,5)                                                               !/*ns        543*/
      if(jz(kz,6).ge.mq(jz(kz,1)+2))go to 680                                   !/*ns        544*/
      jz(kz,6)=jz(kz,6)+1                                                       !/*ns        545*/
      jz(kz-1,6)=mq(kb)                                                         !/*ns        546*/
      jz(kz-2,6)=mq(kk)                                                         !/*ns        547*/
      go to 750                                                                 !/*ns        548*/
!        advance u= lattice element.                                            !/*ns        549*/
  650 ic=namchg(1,abs(mq(kk-2)))                                                !/*ns        550*/
      lp=-mfl(lmfl+1,ic)                                                        !/*ns        551*/
      jc=jz(kz,6)+1                                                             !/*ns        552*/
      do 660 jm=jc,2+laf(lp+1,2)*laf(lp+2,2)*laf(lp+3,2)                        !/*ns        553*/
  660 if(laf(lp+1,jm).eq.abs(mq(kk)))go to 670                                  !/*ns        554*/
      go to 680                                                                 !/*ns        555*/
  670 jz(kz,6)=jm                                                               !/*ns        556*/
      go to 750                                                                 !/*ns        557*/
!        advance to next cell.                                                  !/*ns        558*/
  680 if(jz(mz,6).eq.jz(mz,2))go to 740                                         !/*ns        559*/
      jz(mz,6)=jz(mz,6)+1                                                       !/*ns        560*/
      if(mq(jz(mz,1)+1).ne.1000005)go to 750                                    !/*ns        561*/
!        check if a lattice element varies.                                     !/*ns        562*/
      kz=mz                                                                     !/*ns        563*/
  690 kz=kz+1                                                                   !/*ns        564*/
      if(kz.gt.nz)go to 750                                                     !/*ns        565*/
      if(jz(kz,4).ne.mx)go to 750                                               !/*ns        566*/
      if(jz(kz,2).ne.jz(mz,6))go to 690                                         !/*ns        567*/
      kk=mq(jz(kz,1))                                                           !/*ns        568*/
      if(mq(kk+1).eq.-999.and.mq(kk+2).eq.1000006)go to 700                     !/*ns        569*/
!        initialize lattice elements for this cell.                             !/*ns        570*/
      jz(kz,6)=mq(kk)                                                           !/*ns        571*/
      if(jz(kz,5).eq.0)go to 730                                                !/*ns        572*/
      kz=jz(kz,5)                                                               !/*ns        573*/
      jz(kz,6)=mq(jz(kz,1))                                                     !/*ns        574*/
      if(jz(kz,5).eq.0)go to 730                                                !/*ns        575*/
      kz=jz(kz,5)                                                               !/*ns        576*/
      jz(kz,6)=mq(jz(kz,1))                                                     !/*ns        577*/
      go to 730                                                                 !/*ns        578*/
!        special treatment for lattice (u=).                                    !/*ns        579*/
  700 ic=namchg(1,abs(mq(kk-2)))                                                !/*ns        580*/
      lp=-mfl(lmfl+1,ic)                                                        !/*ns        581*/
      do 710 jm=3,2+laf(lp+1,2)*laf(lp+2,2)*laf(lp+3,2)                         !/*ns        582*/
  710 if(laf(lp+1,jm).eq.abs(mq(kk)))go to 720                                  !/*ns        583*/
  720 jz(kz,6)=jm                                                               !/*ns        584*/
  730 continue                                                                  !/*ns        585*/
      go to 750                                                                 !/*ns        586*/
!        advance level to next level with path indexes that vary.               !/*ns        587*/
  740 mx=mx+1                                                                   !/*ns        588*/
      if(mx.gt.j2)call erprnt(1,1,0,0,0,0,0,0,                                  !/*ns        589*/
     & '53hunable to split global cell path given on si/ds card.')              !/*ns        590*/
      if(mx.gt.j2)go to 890                                                     !/*ns        591*/
      mz=nv(mx,2)                                                               !/*ns        592*/
      if(mz.eq.0)go to 740                                                      !/*ns        593*/
!        advance index in this level and initialize all levels below.           !/*ns        594*/
!        go to the "do 470" loop to initialize and then at "480"                !/*ns        595*/
!        advance next index at this level and then go to 610                    !/*ns        596*/
      ju=mx-1                                                                   !/*ns        597*/
      ms=mx                                                                     !/*ns        598*/
      go to 550                                                                 !/*ns        599*/
  750 if(ms.ne.0)mx=ms                                                          !/*ns        600*/
!        write out one level at a time starting with highest as j.              !/*ns        601*/
      do 830 j9=1,j2                                                            !/*ns        602*/
      j=j2-(j9-1)                                                               !/*ns        603*/
      kk=nv(j,1)                                                                !/*ns        604*/
      jf=0                                                                      !/*ns        605*/
      if(nv(j,2).ne.0)go to 760                                                 !/*ns        606*/
      kcp(kj)=mq(kk)                                                            !/*ns        607*/
      kj=kj+1                                                                   !/*ns        608*/
      ks=kk+2                                                                   !/*ns        609*/
      kk=kk+1                                                                   !/*ns        610*/
      if(kk.eq.ii-i)go to 830                                                   !/*ns        611*/
      if(mq(kk).ne.1000005)go to 830                                            !/*ns        612*/
      go to 780                                                                 !/*ns        613*/
  760 mz=nv(j,2)                                                                !/*ns        614*/
      kk=jz(mz,6)                                                               !/*ns        615*/
      kcp(kj)=mq(kk)                                                            !/*ns        616*/
      kj=kj+1                                                                   !/*ns        617*/
      kk=kk+1                                                                   !/*ns        618*/
      if(kk.eq.ii-i)go to 830                                                   !/*ns        619*/
      if(mq(kk).ne.1000005)go to 830                                            !/*ns        620*/
      ks=kk-1                                                                   !/*ns        621*/
      kz=mz                                                                     !/*ns        622*/
  770 kz=kz+1                                                                   !/*ns        623*/
      if(kz.gt.nz)go to 830                                                     !/*ns        624*/
      if(jz(kz,4).ne.j)go to 830                                                !/*ns        625*/
      if(jz(kz,2).ne.ks)go to 770                                               !/*ns        626*/
      jf=1                                                                      !/*ns        627*/
      if(mq(kk+2).eq.-999.and.mq(kk+3).eq.1000006)go to 820                     !/*ns        628*/
!        deal with a lattice element.                                           !/*ns        629*/
  780 kcp(kj)=1000001                                                           !/*ns        630*/
      kj=kj+1                                                                   !/*ns        631*/
      kk=kk+1                                                                   !/*ns        632*/
      if(mq(kk+1).eq.1000006)go to 810                                          !/*ns        633*/
      kcp(kj)=mq(kk)                                                            !/*ns        634*/
      kj=kj+1                                                                   !/*ns        635*/
      if(jf.eq.0.and.mq(kk+1).eq.1000003)kk=kk+2                                !/*ns        636*/
      if(jf.eq.0)go to 790                                                      !/*ns        637*/
      if(jz(kz,1).ne.kk.and.mq(kk+1).eq.1000003)kk=kk+2                         !/*ns        638*/
      if(jz(kz,1).ne.kk)jf=2                                                    !/*ns        639*/
      if(jz(kz,1).ne.kk)go to 790                                               !/*ns        640*/
      kcp(kj-1)=jz(kz,6)                                                        !/*ns        641*/
      if(jz(kz,5).ne.0)jf=2                                                     !/*ns        642*/
      if(jz(kz,5).ne.0)kz=kz+1                                                  !/*ns        643*/
      if(mq(kk+1).eq.1000003)kk=kk+2                                            !/*ns        644*/
  790 kk=kk+1                                                                   !/*ns        645*/
      kcp(kj)=mq(kk)                                                            !/*ns        646*/
      kj=kj+1                                                                   !/*ns        647*/
      if(jf.ne.2.and.mq(kk+1).eq.1000003)kk=kk+2                                !/*ns        648*/
      if(jf.ne.2)go to 800                                                      !/*ns        649*/
      if(jz(kz,1).ne.kk.and.mq(kk+1).eq.1000003)kk=kk+2                         !/*ns        650*/
      if(jz(kz,1).ne.kk)jf=3                                                    !/*ns        651*/
      if(jz(kz,1).ne.kk)go to 800                                               !/*ns        652*/
      kcp(kj-1)=jz(kz,6)                                                        !/*ns        653*/
      if(jz(kz,5).ne.0)jf=3                                                     !/*ns        654*/
      if(jz(kz,5).ne.0)kz=kz+1                                                  !/*ns        655*/
      if(mq(kk+1).eq.1000003)kk=kk+2                                            !/*ns        656*/
  800 kk=kk+1                                                                   !/*ns        657*/
      kcp(kj)=mq(kk)                                                            !/*ns        658*/
      kj=kj+1                                                                   !/*ns        659*/
      kcp(kj)=1000002                                                           !/*ns        660*/
      kj=kj+1                                                                   !/*ns        661*/
      if(jf.ne.3.and.mq(kk+1).eq.1000003)kk=kk+2                                !/*ns        662*/
      if(jf.ne.3)kk=kk+1                                                        !/*ns        663*/
      if(jf.ne.3)go to 830                                                      !/*ns        664*/
      kcp(kj-2)=jz(kz,6)                                                        !/*ns        665*/
      if(mq(kk+1).eq.1000003)kk=kk+2                                            !/*ns        666*/
      kk=kk+1                                                                   !/*ns        667*/
      go to 830                                                                 !/*ns        668*/
!        lattice element uses only one locator, expand to 3.                    !/*ns        669*/
  810 ic=namchg(1,abs(mq(ks-2)))                                                !/*ns        670*/
      if(mfl(lmfl+1,ic).ge.0)call erprnt(1,1,0,0,0,0,0,0,                       !/*ns        671*/
     & '59hlattice universe of cell path on si/ds card does not exist.')        !/*ns        672*/
      if(mfl(lmfl+1,ic).ge.0)go to 890                                          !/*ns        673*/
      lp=-mfl(lmfl+1,ic)                                                        !/*ns        674*/
      k8=(mq(ks)-1.)/(laf(lp+1,2)*laf(lp+2,2))                                  !/*ns        675*/
      k8=k8+1                                                                   !/*ns        676*/
      if(k8.gt.laf(lp+3,2))call erprnt(1,1,0,0,0,0,0,0,                         !/*ns        677*/
     & '53hlattice index   of cell path on si/ds card incorrect.')              !/*ns        678*/
      if(k8.gt.laf(lp+3,2))go to 890                                            !/*ns        679*/
      j8=(mq(ks)-(k8-1)*laf(lp+1,2)*laf(lp+2,2)-1.)/laf(lp+1,2)                 !/*ns        680*/
      j8=j8+1                                                                   !/*ns        681*/
      i8=mq(ks)-(k8-1)*laf(lp+1,2)*laf(lp+2,2)-(j8-1)*laf(lp+1,2)               !/*ns        682*/
!        switch index to actual rather than beginning with 1.                   !/*ns        683*/
      i8=i8+laf(lp+1,1)-1                                                       !/*ns        684*/
      j8=j8+laf(lp+2,1)-1                                                       !/*ns        685*/
      k8=k8+laf(lp+3,1)-1                                                       !/*ns        686*/
      kcp(kj)=i8                                                                !/*ns        687*/
      kj=kj+1                                                                   !/*ns        688*/
      kcp(kj)=j8                                                                !/*ns        689*/
      kj=kj+1                                                                   !/*ns        690*/
      kcp(kj)=k8                                                                !/*ns        691*/
      kj=kj+1                                                                   !/*ns        692*/
      kcp(kj)=1000002                                                           !/*ns        693*/
      kj=kj+1                                                                   !/*ns        694*/
      kk=kk+1                                                                   !/*ns        695*/
      go to 830                                                                 !/*ns        696*/
!        deal with u= inside lattice indices.                                   !/*ns        697*/
  820 continue                                                                  !/*ns        698*/
      kk=kk+1                                                                   !/*ns        699*/
      ic=namchg(1,abs(mq(kk-2)))                                                !/*ns        700*/
      if(mfl(lmfl+1,ic).ge.0)call erprnt(1,1,0,0,0,0,0,0,                       !/*ns        701*/
     & '59hlattice universe of cell path on si/ds card does not exist.')        !/*ns        702*/
      if(mfl(lmfl+1,ic).ge.0)go to 890                                          !/*ns        703*/
      lp=-mfl(lmfl+1,ic)                                                        !/*ns        704*/
      ir=jz(kz,6)-2                                                             !/*ns        705*/
      k8=(ir-1.)/(laf(lp+1,2)*laf(lp+2,2))                                      !/*ns        706*/
      k8=k8+1                                                                   !/*ns        707*/
      if(k8.gt.laf(lp+3,2))call erprnt(1,1,0,0,0,0,0,0,                         !/*ns        708*/
     & '53hlattice index   of cell path on si/ds card incorrect.')              !/*ns        709*/
      if(k8.gt.laf(lp+3,2))go to 890                                            !/*ns        710*/
      j8=(ir-(k8-1)*laf(lp+1,2)*laf(lp+2,2)-1.)/laf(lp+1,2)                     !/*ns        711*/
      j8=j8+1                                                                   !/*ns        712*/
      i8=ir-(k8-1)*laf(lp+1,2)*laf(lp+2,2)-(j8-1)*laf(lp+1,2)                   !/*ns        713*/
!        switch index to actual rather than beginning with 1.                   !/*ns        714*/
      i8=i8+laf(lp+1,1)-1                                                       !/*ns        715*/
      j8=j8+laf(lp+2,1)-1                                                       !/*ns        716*/
      k8=k8+laf(lp+3,1)-1                                                       !/*ns        717*/
      kcp(kj)=1000001                                                           !/*ns        718*/
      kj=kj+1                                                                   !/*ns        719*/
      kcp(kj)=i8                                                                !/*ns        720*/
      kj=kj+1                                                                   !/*ns        721*/
      kcp(kj)=j8                                                                !/*ns        722*/
      kj=kj+1                                                                   !/*ns        723*/
      kcp(kj)=k8                                                                !/*ns        724*/
      kj=kj+1                                                                   !/*ns        725*/
      kcp(kj)=1000002                                                           !/*ns        726*/
      kj=kj+1                                                                   !/*ns        727*/
      kk=kk+2                                                                   !/*ns        728*/
  830 continue                                                                  !/*ns        729*/
!        fix count and zero out beyond.                                         !/*ns        730*/
      if(jq.eq.1)kcp(i+1)=kj-i-2                                                !/*ns        731*/
      if(jq.ne.1)kcp(i+1+(jq-1)*ix)=kj-i-2-(jq-1)*ix                            !/*ns        732*/
      ks=kj                                                                     !/*ns        733*/
      do 840 ij=ks,ii+(jq-1)*ix-1                                               !/*ns        734*/
      kcp(kj)=0                                                                 !/*ns        735*/
  840 kj=kj+1                                                                   !/*ns        736*/
!        set end point to identify <[] format for printing.                     !/*ns        737*/
      kcp(ks)=1000008                                                           !/*ns        738*/
      if(jq.eq.1.and.kcp(kj).ne.1000008)call erprnt(1,1,0,0,0,0,0,0,            !/*ns        739*/
     & '53hunable to split global cell path given on si/ds card.')              !/*ns        740*/
      if(jq.eq.1.and.kcp(kj).ne.1000008)go to 890                               !/*ns        741*/
      kcp(kj)=0                                                                 !/*ns        742*/
      kj=kj+1                                                                   !/*ns        743*/
!                                                                               !/*ns        744*/
!        end of loop3                                                           !/*ns        745*/
  880 continue                                                                  !/*ns        746*/
!                                                                               !/*ns        747*/
!        additional cell paths have been written                                !/*ns        748*/
      mkcp=kj-1-lkcp                                                            !/*ns        749*/
      return                                                                    !/*ns        750*/
!                                                                               !/*ns        751*/
!        an error was found.                                                    !/*ns        752*/
  890 return                                                                    !/*ns        753*/
      end                                                                       !/*ns        754*/
