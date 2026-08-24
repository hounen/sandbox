!#ident pieces.F from deck pi                                                   !/*pi          1*/
      subroutine pieces                                                         !/*pi          2*/
!        count the pieces of the current cell.                                  !/*pi          3*/
!        the only purpose of the pieces count is to alert the user in           !/*pi          4*/
!        some instances to look for errors in his geometry.  it is              !/*pi          5*/
!        sometimes incorrect, especially when more than two surfaces            !/*pi          6*/
!        intersect almost at the same point.                                    !/*pi          7*/
      use global_data                                                           !/*pix         1*/
      use imcn_data                                                             !/*pix         2*/
!                                                                               !/*pix         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*pix         4*/
      implicit integer(kindi) (i-n)                                             !/*pix         5*/
!                                                                               !/*pix         6*/
      dimension rs(4)                                                           !/*pi         10*/
!                                                                               !/*pi         11*/
!        put the corners in increasing order of s, then r.                      !/*pi         12*/
      do 30 i=1,ncrn-1                                                          !/*pi         13*/
      do 20 j=i,ncrn-1                                                          !/*pi         14*/
      if(rscrn(lrsc+2,j+1).gt.rscrn(lrsc+2,i).or.rscrn(lrsc+2,j+1).eq.          !/*pi         15*/
     & rscrn(lrsc+2,i).and.rscrn(lrsc+1,j+1).gt.rscrn(lrsc+1,i))go to 20        !/*pi         16*/
      do 10 k=1,2                                                               !/*pi         17*/
      a=rscrn(lrsc+k,i)                                                         !/*pi         18*/
      rscrn(lrsc+k,i)=rscrn(lrsc+k,j+1)                                         !/*pi         19*/
      rscrn(lrsc+k,j+1)=a                                                       !/*pi         20*/
      n=icrn(licr+k,i)                                                          !/*pi         21*/
      icrn(licr+k,i)=icrn(licr+k,j+1)                                           !/*pi         22*/
   10 icrn(licr+k,j+1)=n                                                        !/*pi         23*/
   20 continue                                                                  !/*pi         24*/
   30 continue                                                                  !/*pi         25*/
!                                                                               !/*pi         26*/
!        clear corner piece labels.  initialize corner and piece.               !/*pi         27*/
      do 40 i=1,ncrn                                                            !/*pi         28*/
   40 icrn(licr+3,i)=0                                                          !/*pi         29*/
      ij=1                                                                      !/*pi         30*/
      np=0                                                                      !/*pi         31*/
!                                                                               !/*pi         32*/
!        set up surface and branch for the next piece.                          !/*pi         33*/
   50 np=np+1                                                                   !/*pi         34*/
      nc=10000*np+5000                                                          !/*pi         35*/
      js=icrn(licr+1,ij)                                                        !/*pi         36*/
      jh=1                                                                      !/*pi         37*/
!                                                                               !/*pi         38*/
!        label the corner and set up the next surface.                          !/*pi         39*/
   60 ic=ij                                                                     !/*pi         40*/
      nc=nc+1                                                                   !/*pi         41*/
      icrn(licr+3,ic)=nc                                                        !/*pi         42*/
      js=icrn(licr+1,ic)+icrn(licr+2,ic)-js                                     !/*pi         43*/
!                                                                               !/*pi         44*/
!        set branch indicator.                                                  !/*pi         45*/
      if(js.ne.0)rb=-.5*scfq(lscq+2,js)                                         !/*pi         46*/
      jh=3-jh                                                                   !/*pi         47*/
      if(icrn(licr+1,ic).eq.icrn(licr+2,ic).or.js.eq.0)go to 70                 !/*pi         48*/
      jh=1                                                                      !/*pi         49*/
      if(scfq(lscq+1,js)*scfq(lscq+2,js).ne.0..and.                             !/*pi         50*/
     & rscrn(lrsc+1,ic).lt.rb)jh=2                                              !/*pi         51*/
!                                                                               !/*pi         52*/
!        find the next corner of this piece.  look right, then left.            !/*pi         53*/
   70 do 110 jd=1,2                                                             !/*pi         54*/
      ij=ic                                                                     !/*pi         55*/
   80 ij=ij+3-2*jd                                                              !/*pi         56*/
      if(ij.gt.ncrn.or.ij.lt.1)go to 110                                        !/*pi         57*/
      if(rscrn(lrsc+1,ij).eq.rscrn(lrsc+1,ic).and.                              !/*pi         58*/
     & rscrn(lrsc+2,ij).eq.rscrn(lrsc+2,ic).or.                                 !/*pi         59*/
     & icrn(licr+1,ij).ne.js.and.icrn(licr+2,ij).ne.js.or.                      !/*pi         60*/
     & icrn(licr+3,ij).gt.0.and.icrn(licr+3,ij).lt.10000*np)go to 80            !/*pi         61*/
      if(js.eq.0)go to 90                                                       !/*pi         62*/
      if(icrn(licr+1,ij).ne.icrn(licr+2,ij).and.                                !/*pi         63*/
     & scfq(lscq+1,js).ne.0..and.scfq(lscq+2,js).ne.0..and.                     !/*pi         64*/
     & (rscrn(lrsc+1,ij)-rb)*(3-2*jh).lt.0.)go to 80                            !/*pi         65*/
!                                                                               !/*pi         66*/
!        check that the midpoint of the section is on the cell boundary.        !/*pi         67*/
   90 do 100 i=1,2                                                              !/*pi         68*/
      rs(i)=rscrn(lrsc+i,ic)                                                    !/*pi         69*/
  100 rs(2+i)=rscrn(lrsc+i,ij)                                                  !/*pi         70*/
      call midpnt(js,rs,rs(3),jh,l,m1)                                          !/*pi         71*/
      if(l.eq.1)go to 130                                                       !/*pi         72*/
  110 continue                                                                  !/*pi         73*/
      if(dbcn(7).ne.0.)write(iuo,120)                                           !/*pi         74*/
  120 format(/41h the current cell appears to be infinite.)                     !/*pi         75*/
      return                                                                    !/*pi         76*/
!                                                                               !/*pi         77*/
!        check whether the piece is closed yet.                                 !/*pi         78*/
  130 if(icrn(licr+3,ij).eq.0)go to 60                                          !/*pi         79*/
!                                                                               !/*pi         80*/
!        find a corner not yet labeled.  return if there is none.               !/*pi         81*/
      do 140 ij=1,ncrn                                                          !/*pi         82*/
  140 if(icrn(licr+3,ij).eq.0)go to 50                                          !/*pi         83*/
      vols(lvls+2,icl)=np                                                       !/*pi         84*/
      return                                                                    !/*pi         85*/
      end                                                                       !/*pi         86*/
