!#ident mapmaz.F from deck zm4b                                                 !/*zm4b        1*/
      subroutine mapmaz(ip)                                                     !/*zm4b        2*/
!        print the number of particles starting, entering, or colliding         !/*zm4b        3*/
!        in each element of the repeated structures / lattice maze              !/*zm4b        4*/
!        and/or count the number of non-scoring kcode fission elements.         !/*zm4b        5*/
      use global_data                                                           !/*zmx         1*/
!                                                                               !/*zmx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*zmx         3*/
      implicit integer(kindi) (i-n)                                             !/*zmx         4*/
!                                                                               !/*zmx         5*/
      dimension ic(0:mxlv),ie(0:mxlv),li(15,0:mxlv),lp(0:mxlv),                 !/*zm4b        7*/
     & lz(0:mxlv),ms(3),ne(0:mxlv)                                              !/*zm4b        8*/
      character hc(2)*10,hf*1,hs(95)*1                                          !/*zmx         6*/
      data hc/'collisions',' substeps'/                                         !/*zmx         7*/
!                                                                               !/*zm4b       11*/
!        initialize and print table heading.                                    !/*zm4b       12*/
      if(nmaz.eq.0.and.nmzu.ne.0)call erprnt(1,2,0,0,0,0,0,0,                   !/*zm4b       13*/
     & '53hcannot print table 128 --- turned off in initial run.')              !/*zm4b       14*/
      if(nmaz.eq.0)return                                                       !/*zm4b       15*/
      lz(0)=nmaz*3*(ip-nmip)+1                                                  !/*zm4b       16*/
      if(ink(128).ne.0.and.charge(ip).eq.zero)write(iuo,10)hnp(ip),hc(1)        !/*zmx         8*/
      if(ink(128).ne.0.and.charge(ip).ne.zero)write(iuo,10)hnp(ip),hc(2)        !/*zmx         9*/
   10 format(1h1,a11,47h activity in each repeated structure / lattice ,        !/*zmx        10*/
     & 7helement,38x,15hprint table 128//7x,6hsource,4x,8hentering,2x,          !/*zmx        11*/
     & a10,8x,4hpath/)                                                          !/*zm4b       20*/
      do 20 i=1,3                                                               !/*zm4b       21*/
   20 ms(i)=0                                                                   !/*zm4b       22*/
      do 30 i=0,mxlv                                                            !/*zm4b       23*/
   30 li(15,i)=0                                                                !/*zm4b       24*/
!                                                                               !/*zm4b       25*/
!        get locator to universe 0 in mazu array.                               !/*zm4b       26*/
      do 40 i=1,mxa                                                             !/*zm4b       27*/
   40 if(jun(ljun+i).eq.0)go to 50                                              !/*zm4b       28*/
   50 lp(0)=mazp(lmzp+2,i)                                                      !/*zm4b       29*/
      lv=-1                                                                     !/*zm4b       30*/
!                                                                               !/*zm4b       31*/
!        loop thru all levels in and below universe 0.                          !/*zm4b       32*/
   60 lv=lv+1                                                                   !/*zm4b       33*/
      if(lp(lv).gt.nmzu.or.lp(lv).le.0.or.lz(lv).le.nmaz*3*(ip-nmip).or.        !/*zm4b       34*/
     & lz(lv).gt.nmaz*3*(ip-nmip)+nmaz)call expirx(1,'mapmaz','trouble')        !/*zm4b       35*/
      ne(lv)=mazu(lmzu+lp(lv))                                                  !/*zm4b       36*/
      ie(lv)=0                                                                  !/*zm4b       37*/
   70 ie(lv)=ie(lv)+1                                                           !/*zm4b       38*/
      if(mazu(lmzu+lp(lv)-2).eq.0)go to 80                                      !/*zm4b       39*/
!                                                                               !/*zm4b       40*/
!        finite lattice.                                                        !/*zm4b       41*/
      ic(lv)=mazu(lmzu+lp(lv)-2)                                                !/*zm4b       42*/
      jc=mazu(lmzu+lp(lv)+ne(lv)+ie(lv))                                        !/*zm4b       43*/
      if(jc.eq.0)go to 130                                                      !/*zm4b       44*/
      if(jc.eq.ic(lv))go to 90                                                  !/*zm4b       45*/
      lp(lv+1)=mazp(lmzp+2,jc)                                                  !/*zm4b       46*/
      lz(lv+1)=lz(lv)+mazu(lmzu+lp(lv)+ie(lv))                                  !/*zm4b       47*/
      go to 60                                                                  !/*zm4b       48*/
!                                                                               !/*zm4b       49*/
!        repeated structure / infinite lattice / simple cell.                   !/*zm4b       50*/
   80 ic(lv)=mazu(lmzu+lp(lv)+ne(lv)+ie(lv))                                    !/*zm4b       51*/
      if(mfl(lmfl+1,ic(lv)).eq.0)go to 90                                       !/*zm4b       52*/
      lp(lv+1)=mazp(lmzp+3,ic(lv))                                              !/*zm4b       53*/
      lz(lv+1)=lz(lv)+mazu(lmzu+lp(lv)+ie(lv))                                  !/*zm4b       54*/
      go to 60                                                                  !/*zm4b       55*/
!                                                                               !/*zm4b       56*/
!        end of path.  write source, entering, collisions and path.             !/*zm4b       57*/
!        and/or count non-scoring kcode fission elements.                       !/*zm4b       58*/
   90 hf=' '                                                                    !/*zm4b       59*/
      mz=lz(lv)+mazu(lmzu+lp(lv)+ie(lv))                                        !/*zm4b       60*/
      if(nsr.ne.71)go to 110                                                    !/*zm4b       61*/
      if(ip.ne.1)go to 110                                                      !/*zm4b       62*/
      if(lfcl(llfc+ic(lv)).eq.0)go to 110                                       !/*zm4b       63*/
      hf='*'                                                                    !/*zm4b       64*/
      do 100 i=1,3                                                              !/*zm4b       65*/
  100 if(maze(lmaz+mz+(i-1)*nmaz).eq.0)mazf(i)=mazf(i)+1                        !/*zm4b       66*/
  110 if(ink(128).eq.0)go to 130                                                !/*zm4b       67*/
      call pathmz(n,lv,ic,ie,li,hs)                                             !/*zm4b       68*/
      write(iuo,'(1x,i11,a1,i11,i12,95a1)')maze(lmaz+mz),hf,                    !/*zm4b       69*/
     & maze(lmaz+mz+nmaz),maze(lmaz+mz+2*nmaz),(hs(i),i=1,n)                    !/*zm4b       70*/
      do 120 i=1,3                                                              !/*zm4b       71*/
  120 ms(i)=ms(i)+maze(lmaz+mz+(i-1)*nmaz)                                      !/*zm4b       72*/
  130 if(ie(lv).lt.ne(lv))go to 70                                              !/*zm4b       73*/
      kp=(100*mz)/nmaz                                                          !/*zm4b       74*/
#ifdef PCDOS                                                                    /*!zm4b       75*/
      if(lockl)call pttyin                                                      !/*zm4b       76*/
#endif                                                                          /*!cpp       282*/
      if(irup.ne.0)call qttyin(kp,'16hprint table 128.,i7,1h%')                 !/*zm4b       77*/
      lv=lv-1                                                                   !/*zm4b       78*/
      if(lv.ge.0)go to 130                                                      !/*zm4b       79*/
!                                                                               !/*zm4b       80*/
!       various warnings and explanations.                                      !/*zm4b       81*/
      if(ink(128).eq.0)return                                                   !/*zm4b       82*/
      write(iuo,'(/3i12/)')(ms(i),i=1,3)                                        !/*zm4b       83*/
      if(ip.eq.1.and.nsr.eq.71)write(iuo,140)                                   !/*zm4b       84*/
  140 format(10x,36h* = kcode fissionable material cell./)                      !/*zm4b       85*/
      if(ip.eq.2.and.nmip.eq.1)write(iuo,150)                                   !/*zm4b       86*/
  150 format(10x,46hphoton source is from neutron-induced photons./)            !/*zm4b       87*/
      do 160 i=1,mxa                                                            !/*zm4b       88*/
  160 if(lat(llat+1,i).ne.0.and.mfl(lmfl+1,i).ge.0)write(iuo,170)               !/*zm4b       89*/
     & ncl(lncl+i)                                                              !/*zm4b       90*/
  170 format(46h cannot guarantee sampling of all elements in ,                 !/*zm4b       91*/
     & 21hinfinite lattice cell,i5)                                             !/*zm4b       92*/
      mt=0                                                                      !/*zm4b       93*/
#if defined(MULTT) || defined(MULTP)                                            /*!zm4b       94*/
      mt=mtasks                                                                 !/*zm4b       95*/
#endif                                                                          /*!cpp       283*/
      if(ip.eq.nmip.and.mct.ge.0)write(iuo,180)nmzu+3*mxa+                      !/*zm4b       96*/
     & 3*(mt+1)*nmaz*(kpt(1)+kpt(2)+kpt(3))                                     !/*zm4b       97*/
  180 format(/10x,24hprint table 128 requires,i12,                              !/*zm4b       98*/
     & 48h decimal words of dynamically allocated storage.)                     !/*zm4b       99*/
      return                                                                    !/*zm4b      100*/
      end                                                                       !/*zm4b      101*/
