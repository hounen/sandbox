!#ident unimaz.F from deck zu4b                                                 !/*zu4b        1*/
      subroutine unimaz                                                         !/*zu4b        2*/
!        set up arrays for repeated structures / lattice universe map.          !/*zu4b        3*/
!        called from pass1 (to get nmaz and nmzu) and igeom.                    !/*zu4b        4*/
      use allocate_memory                                                       !/*zux         1*/
      use global_data                                                           !/*zux         2*/
!                                                                               !/*zux         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*zux         4*/
      implicit integer(kindi) (i-n)                                             !/*zux         5*/
!                                                                               !/*zux         6*/
!                                                                               !/*zu4b        6*/
!        each cell, ic, is filled with either a universe that is a              !/*zu4b        7*/
!        lattice or a universe that is a list of cells.                         !/*zu4b        8*/
!                                                                               !/*zu4b        9*/
!        set up the following arrays:                                           !/*zu4b       10*/
!        nmzu = length of mazu array. (approximate in 1st pass)                 !/*zu4b       11*/
!        nmzu = length of mazu array. (in 1st pass = max(mxa,n)                 !/*zu4b       12*/
!               where n=max size of any finite lattice)                         !/*zu4b       13*/
!        nmaz = length of maze array. ( = 0 in 1st pass)                        !/*zu4b       14*/
!        mazp(1,ic) = i, index of cell ic in mazu(j) list.                      !/*zu4b       15*/
!        mazp(2,ic) = universe address j of cell ic.                            !/*zu4b       16*/
!        mazp(3,ic) = address j of universe filling cell ic.                    !/*zu4b       17*/
!        mazu(j-3) = i = universe name.                                         !/*zu4b       18*/
!        mazu(j-2) = finite lattice cell filling universe i.                    !/*zu4b       19*/
!        mazu(j-1) = total number of lowest level elements below u=i.           !/*zu4b       20*/
!        mazu(j)   = ne = number of cells/elements in universe i                !/*zu4b       21*/
!        mazu(j+k) = number of elements below kth cell/universe.                !/*zu4b       22*/
!                    later changed to offsets in maze array.                    !/*zu4b       23*/
!        mazu(j+ne+k) = kth cell in universe i (repeated structures)            !/*zu4b       24*/
!        mazu(j+ne+k) = first cell of universe filling kth lat element          !/*zu4b       25*/
!                                                                               !/*zu4b       26*/
!        set up all but pointers and subelements of mazp and mazu.              !/*zu4b       27*/
!                                                                               !/*zu4b       28*/
!        iu = current universe name.                                            !/*zu4b       29*/
!        lp = current universe address.                                         !/*zu4b       30*/
!        ic = current cell.                                                     !/*zu4b       31*/
!        kf = number of finished universes.                                     !/*zu4b       32*/
!        ke = number of finished elements in this universe.                     !/*zu4b       33*/
!                                                                               !/*zu4b       34*/
!        special initialization if called from pass1 (nmaz=0).                  !/*zu4b       35*/
      if(nmaz.ne.0)go to 20                                                     !/*zu4b       36*/
      if(ink(128).eq.0.and.mnk.le.0)call erprnt(1,2,0,0,0,0,0,0,                !/*zu4b       37*/
     & '40huniverse map (print table 128) disabled.')                           !/*zu4b       38*/
      if(ink(128).eq.0.and.mnk.le.0)return                                      !/*zu4b       39*/
      nmzu=max(nmzu,mxa)                                                        !/*zux         7*/
      lmz=3*max(nmzu,2000)                                                      !/*zux         8*/
      mazu => chg_mem (mazu,lmz,is)                                             !/*zux         9*/
      if(is.ne.0)call expire(0,'unimaz','chg_mem error')                        !/*zux        10*/
      mazu=0                                                                    !/*zux        11*/
!                                                                               !/*zu4b       46*/
!        loop over all cells and process all universes not already done.        !/*zu4b       47*/
   20 kf=0                                                                      !/*zu4b       48*/
      lp=4                                                                      !/*zu4b       49*/
      do 140 ic=1,mxa                                                           !/*zu4b       50*/
      if(mazp(lmzp+2,ic).ne.0)go to 140                                         !/*zu4b       51*/
      iu=abs(jun(ljun+ic))                                                      !/*zu4b       52*/
!                                                                               !/*zu4b       53*/
!        get mazp(3,jc): cell jc is filled with this universe.                  !/*zu4b       54*/
      if(iu.eq.0)go to 40                                                       !/*zu4b       55*/
      do 30 jc=1,mxa                                                            !/*zu4b       56*/
   30 if(mfl(lmfl+1,jc).eq.iu)mazp(lmzp+3,jc)=lp                                !/*zu4b       57*/
!                                                                               !/*zu4b       58*/
!        process universe iu.                                                   !/*zu4b       59*/
   40 mazu(lmzu+lp-3)=iu                                                        !/*zu4b       60*/
      ke=0                                                                      !/*zu4b       61*/
      if(mfl(lmfl+1,ic).ge.0)go to 70                                           !/*zu4b       62*/
!                                                                               !/*zu4b       63*/
!        finite lattice.                                                        !/*zu4b       64*/
      mf=-mfl(lmfl+1,ic)                                                        !/*zu4b       65*/
      mazu(lmzu+lp-2)=ic                                                        !/*zu4b       66*/
      ne=max(1,laf(mf+1,2)*laf(mf+2,2)*laf(mf+3,2))                             !/*zu4b       67*/
      mazp(lmzp+2,ic)=lp                                                        !/*zu4b       68*/
      mazu(lmzu+lp)=ne                                                          !/*zu4b       69*/
      do 60 ie=1,ne                                                             !/*zu4b       70*/
      ju=laf(mf+1,2+ie)                                                         !/*zu4b       71*/
      if(ju.eq.0)go to 50                                                       !/*zu4b       72*/
      if(ju.ne.iu)go to 60                                                      !/*zu4b       73*/
      mazu(lmzu+lp+ne+ie)=ic                                                    !/*zu4b       74*/
      mazu(lmzu+lp+ie)=1                                                        !/*zu4b       75*/
   50 ke=ke+1                                                                   !/*zu4b       76*/
   60 continue                                                                  !/*zu4b       77*/
      go to 100                                                                 !/*zu4b       78*/
!                                                                               !/*zu4b       79*/
!        repeated structure, infinite lattice, or simple cell.                  !/*zu4b       80*/
   70 ne=0                                                                      !/*zu4b       81*/
      do 80 jc=ic,mxa                                                           !/*zu4b       82*/
      if(abs(jun(ljun+jc)).ne.iu)go to 80                                       !/*zu4b       83*/
      ne=ne+1                                                                   !/*zu4b       84*/
      mazu(lmzu+lp+ne)=jc                                                       !/*zu4b       85*/
      mazp(lmzp+2,jc)=lp                                                        !/*zu4b       86*/
      mazp(lmzp+1,jc)=ne                                                        !/*zu4b       87*/
   80 continue                                                                  !/*zu4b       88*/
      mazu(lmzu+lp)=ne                                                          !/*zu4b       89*/
!                                                                               !/*zu4b       90*/
!        shift mazu(j+k) to mazu(j+ne+k) and fill mazu(j+k) in simple           !/*zu4b       91*/
!        (unfilled) case.                                                       !/*zu4b       92*/
      do 90 i=1,ne                                                              !/*zu4b       93*/
      jc=mazu(lmzu+lp+i)                                                        !/*zu4b       94*/
      mazu(lmzu+lp+ne+i)=jc                                                     !/*zu4b       95*/
      mazu(lmzu+lp+i)=0                                                         !/*zu4b       96*/
      if(mfl(lmfl+1,jc).ne.0)go to 90                                           !/*zu4b       97*/
      mazu(lmzu+lp+i)=1                                                         !/*zu4b       98*/
      ke=ke+1                                                                   !/*zu4b       99*/
   90 continue                                                                  !/*zu4b      100*/
  100 if(ke.ne.ne)go to 120                                                     !/*zu4b      101*/
!                                                                               !/*zu4b      102*/
!        convert sizes to offsets for completely filled cells.                  !/*zu4b      103*/
      lz=0                                                                      !/*zu4b      104*/
      do 110 i=1,ne                                                             !/*zu4b      105*/
      ls=mazu(lmzu+lp+i)                                                        !/*zu4b      106*/
      mazu(lmzu+lp+i)=lz                                                        !/*zu4b      107*/
  110 lz=lz+ls                                                                  !/*zu4b      108*/
      mazu(lmzu+lp-1)=lz                                                        !/*zu4b      109*/
      kf=kf+1                                                                   !/*zu4b      110*/
!                                                                               !/*zu4b      111*/
  120 lp=lp+2*ne+4                                                              !/*zu4b      112*/
      if(nmaz.gt.0)go to 140                                                    !/*zu4b      113*/
      if(lp+2*nmzu.gt.lmz)then                                                  !/*zux        12*/
        mazu => chg_mem (mazu,lmz+3*max(nmzu,lmz/10,2000),is)                   !/*zux        13*/
        if(is.ne.0)call expire(0,'unimaz','chg_mem error')                      !/*zux        14*/
      endif                                                                     !/*zux        15*/
  140 continue                                                                  !/*zu4b      120*/
!                                                                               !/*zu4b      121*/
      if(nmaz.gt.0.and.lp-4.gt.nmzu)call erprnt(1,1,2,nmzu,lp-4,0,0,0,          !/*zu4b      122*/
     & '31hunimaz nmzu dimension overflow.,2i10')                               !/*zu4b      123*/
      if(nmaz.eq.0)nmzu=lp-4                                                    !/*zu4b      124*/
      if(kf.eq.0.and.nmaz.gt.0)call erprnt(1,1,0,0,0,0,0,0,                     !/*zu4b      125*/
     & '36hno completely filled universe found.')                               !/*zu4b      126*/
!                                                                               !/*zu4b      127*/
!        step through mazu array to determine how many elements or              !/*zu4b      128*/
!        cells fill each universe, mazu(j-1), and the number of                 !/*zu4b      129*/
!        subelements, mazu(j+i) in each element i.                              !/*zu4b      130*/
!                                                                               !/*zu4b      131*/
!        kl = number of elements finished this pass.                            !/*zu4b      132*/
  150 kl=0                                                                      !/*zu4b      133*/
      lp=4                                                                      !/*zu4b      134*/
!                                                                               !/*zu4b      135*/
!        process next universe.                                                 !/*zu4b      136*/
  160 ne=mazu(lmzu+lp)                                                          !/*zu4b      137*/
      if(mazu(lmzu+lp-1).ne.0)go to 300                                         !/*zu4b      138*/
      ke=0                                                                      !/*zu4b      139*/
      ic=mazu(lmzu+lp-2)                                                        !/*zu4b      140*/
      if(ic.eq.0)go to 250                                                      !/*zu4b      141*/
!                                                                               !/*zu4b      142*/
!        finite lattice universe.                                               !/*zu4b      143*/
      mf=-mfl(lmfl+1,ic)                                                        !/*zu4b      144*/
      jc=1                                                                      !/*zu4b      145*/
      do 240 ie=1,ne                                                            !/*zu4b      146*/
      if(mazu(lmzu+lp+ie).ne.0)go to 230                                        !/*zu4b      147*/
      ju=laf(mf+1,2+ie)                                                         !/*zu4b      148*/
      if(ju.eq.0)go to 230                                                      !/*zu4b      149*/
      if(ju.ne.mazu(lmzu+lp-3))go to 170                                        !/*zu4b      150*/
      mazu(lmzu+lp+ne+ie)=ic                                                    !/*zu4b      151*/
      mazu(lmzu+lp+ie)=1                                                        !/*zu4b      152*/
      go to 220                                                                 !/*zu4b      153*/
  170 if(mazu(lmzu+lp+ne+ie).ne.0)go to 200                                     !/*zu4b      154*/
      if(abs(jun(ljun+jc)).eq.ju)go to 190                                      !/*zu4b      155*/
      do 180 jc=1,mxa                                                           !/*zu4b      156*/
  180 if(abs(jun(ljun+jc)).eq.ju)go to 190                                      !/*zu4b      157*/
      if(nmaz.gt.0)call erprnt(1,1,2,ju,ncl(lncl+ic),0,0,0,                     !/*zu4b      158*/
     & '20hcannot find universe,i5,21h filling lattice cell,i5')                !/*zu4b      159*/
      go to 230                                                                 !/*zu4b      160*/
  190 mazu(lmzu+lp+ne+ie)=jc                                                    !/*zu4b      161*/
      go to 210                                                                 !/*zu4b      162*/
  200 jc=mazu(lmzu+lp+ne+ie)                                                    !/*zu4b      163*/
  210 mp=mazp(lmzp+2,jc)                                                        !/*zu4b      164*/
      if(mazu(lmzu+mp-1).eq.0)go to 240                                         !/*zu4b      165*/
      mazu(lmzu+lp+ie)=mazu(lmzu+mp-1)                                          !/*zu4b      166*/
  220 kl=kl+1                                                                   !/*zu4b      167*/
  230 ke=ke+1                                                                   !/*zu4b      168*/
  240 continue                                                                  !/*zu4b      169*/
      go to 280                                                                 !/*zu4b      170*/
!                                                                               !/*zu4b      171*/
!        repeated structure or infinite lattice or simple universe.             !/*zu4b      172*/
  250 do 270 ie=1,ne                                                            !/*zu4b      173*/
      if(mazu(lmzu+lp+ie).ne.0)go to 260                                        !/*zu4b      174*/
      jc=mazu(lmzu+lp+ne+ie)                                                    !/*zu4b      175*/
      mp=mazp(lmzp+3,jc)                                                        !/*zu4b      176*/
      if(mazu(lmzu+mp-1).eq.0)go to 270                                         !/*zu4b      177*/
      mazu(lmzu+lp+ie)=mazu(lmzu+mp-1)                                          !/*zu4b      178*/
      kl=kl+1                                                                   !/*zu4b      179*/
  260 ke=ke+1                                                                   !/*zu4b      180*/
  270 continue                                                                  !/*zu4b      181*/
!                                                                               !/*zu4b      182*/
!        universe finished. set mazu(j-1) and convert subelement lengths        !/*zu4b      183*/
!        to cumulative.                                                         !/*zu4b      184*/
  280 if(ke.ne.ne)go to 300                                                     !/*zu4b      185*/
      kf=kf+1                                                                   !/*zu4b      186*/
      lz=0                                                                      !/*zu4b      187*/
      do 290 k=1,ne                                                             !/*zu4b      188*/
      ls=mazu(lmzu+lp+k)                                                        !/*zu4b      189*/
      mazu(lmzu+lp+k)=lz                                                        !/*zu4b      190*/
  290 lz=lz+ls                                                                  !/*zu4b      191*/
      mazu(lmzu+lp-1)=lz                                                        !/*zu4b      192*/
      if(mazu(lmzu+lp-3).eq.0)go to 310                                         !/*zu4b      193*/
!                                                                               !/*zu4b      194*/
!        go to next unresolved (unknown length) universe.                       !/*zu4b      195*/
  300 lp=lp+2*ne+4                                                              !/*zu4b      196*/
      if(lp.lt.nmzu)go to 160                                                   !/*zu4b      197*/
      if(kl.gt.0)go to 150                                                      !/*zu4b      198*/
      if(nmaz.gt.0)call erprnt(1,1,0,0,0,0,0,0,                                 !/*zu4b      199*/
     & '51himproper geometry specification detected in unimaz.')                !/*zu4c        1*/
!                                                                               !/*zu4b      201*/
!        universe 0 finished.  thus all universes resolved.                     !/*zu4b      202*/
  310 if(lz.eq.0)go to 320                                                      !/*zu4b      203*/
      if(nmaz.eq.0)go to 330                                                    !/*zu4b      204*/
      mt=0                                                                      !/*zu4b      205*/
#if defined(MULTT) || defined(MULTP)                                            /*!zu4b      206*/
      mt=mtasks                                                                 !/*zu4b      207*/
#endif                                                                          /*!cpp       115*/
      l=nmzu+3*mxa+3*(mt+1)*nmaz*(kpt(1)+kpt(2)+kpt(3))                         !/*zu4b      208*/
      if(l.gt.500000)call erprnt(1,2,1,l,0,0,0,0,                               !/*zu4b      209*/
     & '24hprint table 128 requires,i9,23h dec. words of storage.')             !/*zu4b      210*/
      if(lz.le.nmaz)return                                                      !/*zu4b      211*/
      call erprnt(1,1,2,nmaz,lz,0,0,0,                                          !/*zu4b      212*/
     & '31hunimaz nmaz dimension overflow.,2i10')                               !/*zu4b      213*/
      lz=0                                                                      !/*zu4b      214*/
  320 ink(128)=0                                                                !/*zu4b      215*/
  330 nmaz=lz                                                                   !/*zu4b      216*/
      return                                                                    !/*zu4b      217*/
      end                                                                       !/*zu4b      218*/
