!#ident igeom.F from deck ig                                                    !/*ig          1*/
      subroutine igeom                                                          !/*ig          2*/
!        set up the geometry.                                                   !/*ig          3*/
      use global_data                                                           !/*igx         1*/
      use imcn_data                                                             !/*igx         2*/
!                                                                               !/*igx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*igx         4*/
      implicit integer(kindi) (i-n)                                             !/*igx         5*/
!                                                                               !/*igx         6*/
!                                                                               !/*ig4c        1*/
!        expand any macrobody surfaces on cell cards to full surfaces.          !/*ig4c        2*/
      call mbody(2,ix)                                                          !/*ig4c        3*/
!                                                                               !/*ig          6*/
!        transform the coefficients of any surfaces that need it.               !/*ig          7*/
      do 10 js=1,mxj                                                            !/*ig          8*/
   10 if(jtr(ljtr+js).ne.0)call trfsrf(js)                                      !/*ig          9*/
!                                                                               !/*ig         10*/
!        create surfaces for any cells that have trcl entries.                  !/*ig         11*/
      if(junf.eq.0)go to 80                                                     !/*ig         12*/
      do 20 ic=1,mxa                                                            !/*ig         13*/
   20 if(ktr(lktr+ic).ne.0)call dotrcl(ic)                                      !/*ig         14*/
!                                                                               !/*ig         15*/
!        endow each deprived universe with its cell's transformation.           !/*ig         16*/
!        convert mfl(3, ) entries from names to indexes.                        !/*ig         17*/
      do 70 ic=1,mxa                                                            !/*ig         18*/
      if(mfl(lmfl+1,ic).eq.0)go to 70                                           !/*ig         19*/
      if(mfl(lmfl+1,ic).lt.0)go to 40                                           !/*ig         20*/
      if(mfl(lmfl+3,ic).eq.0)mfl(lmfl+3,ic)=ktr(lktr+ic)                        !/*ig         21*/
      if(mfl(lmfl+3,ic).eq.0)go to 70                                           !/*ig         22*/
      l=0                                                                       !/*ig         23*/
      do 30 m=1,mxtr                                                            !/*ig         24*/
   30 if(abs(trf(ltrf+1,m)).eq.mfl(lmfl+3,ic))l=m                               !/*ig         25*/
      if(l.eq.0)call erprnt(1,1,2,mfl(lmfl+3,ic),ncl(lncl+ic),0,0,0,            !/*ig         26*/
     & '14htransformation,i5,17h for fill of cell,i7,11h is absent.')           !/*ig         27*/
      mfl(lmfl+3,ic)=l                                                          !/*ig         28*/
      go to 70                                                                  !/*ig         29*/
   40 lp=-mfl(lmfl+1,ic)                                                        !/*ig         30*/
      do 60 j=3,2+laf(lp+1,2)*laf(lp+2,2)*laf(lp+3,2)                           !/*ig         31*/
      if(laf(lp+1,j).eq.0.or.laf(lp+1,j).eq.abs(jun(ljun+ic)))go to 60          !/*ig         32*/
      if(laf(lp+3,j).eq.0)laf(lp+3,j)=ktr(lktr+ic)                              !/*ig         33*/
      if(laf(lp+3,j).eq.0)go to 60                                              !/*ig         34*/
      l=0                                                                       !/*ig         35*/
      do 50 m=1,mxtr                                                            !/*ig         36*/
   50 if(abs(trf(ltrf+1,m)).eq.laf(lp+3,j))l=m                                  !/*ig         37*/
      if(l.eq.0)call erprnt(1,1,2,laf(lp+3,j),ncl(lncl+ic),0,0,0,               !/*ig         38*/
     & '14htransformation,i5,17h for fill of cell,i7,11h is absent.')           !/*ig         39*/
      laf(lp+3,j)=l                                                             !/*ig         40*/
   60 continue                                                                  !/*ig         41*/
   70 continue                                                                  !/*ig         42*/
!                                                                               !/*ig         43*/
!        expand # operators and check the cells and surfaces.                   !/*ig         44*/
   80 call chekcs                                                               !/*ig         45*/
!                                                                               !/*ig         46*/
!        calculate the constants of any lattices in the geometry.               !/*ig         47*/
      if(nlat.ne.0)call latcon                                                  !/*ig         48*/
!                                                                               !/*ig4b        1*/
!        get the size of the universe map.                                      !/*ig4b        2*/
      if(nmaz.ne.0)call unimaz                                                  !/*ig4b        3*/
      return                                                                    !/*ig         49*/
      end                                                                       !/*ig         50*/
