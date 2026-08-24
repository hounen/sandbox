!#ident pltloc.F from deck puy4c2                                               !/*puy4c2      1*/
      subroutine pltloc(s1,t1)                                                  !/*puy4c2      3*/
#ifdef PLOT                                                                     /*!puyx        1*/
!        get location of plot coordinates s1,t1                                 !/*puy4c2      4*/
      use global_data                                                           !/*puyx        2*/
      use plot_data                                                             !/*puyx        3*/
!                                                                               !/*puyx        4*/
      implicit real(kindr) (a-h,o-z)                                            !/*puyx        5*/
      implicit integer(kindi) (i-n)                                             !/*puyx        6*/
!                                                                               !/*puyx        7*/
      real s1,t1                                                                !/*puy4c2      7*/
!                                                                               !/*puy4c2     10*/
      xxx=origin(1)+s1*basis(1)+t1*basis(4)                                     !/*puy4c2     11*/
      yyy=origin(2)+s1*basis(2)+t1*basis(5)                                     !/*puy4c2     12*/
      zzz=origin(3)+s1*basis(3)+t1*basis(6)                                     !/*puy4c2     13*/
      ju=0                                                                      !/*puy4c2     14*/
      lev=0                                                                     !/*puy4c2     15*/
      jsu=0                                                                     !/*puy4c2     16*/
      if(iap.le.0.or.iap.gt.mxa)go to 90                                        !/*puy4c2     17*/
      if(junf.eq.0)go to 80                                                     !/*puy4c2     18*/
      if(jun(ljun+iap).ne.0)go to 90                                            !/*puy4c2     19*/
   80 call chkcel(iap,0,j)                                                      !/*puy4c2     20*/
      if(j.eq.0)return                                                          !/*puy4c2     21*/
   90 do 140 iap=1,mxa                                                          !/*puy4c2     22*/
      if(junf.eq.0)go to 100                                                    !/*puy4c2     23*/
      if(abs(jun(ljun+iap)).ne.ju)go to 140                                     !/*puy4c2     24*/
      if(lat(llat+1,iap).eq.0)go to 100                                         !/*puy4c2     25*/
      call findel(iap)                                                          !/*puy4c2     26*/
      go to 110                                                                 !/*puy4c2     27*/
  100 call chkcel(iap,0,j)                                                      !/*puy4c2     28*/
      if(j.ne.0)go to 140                                                       !/*puy4c2     29*/
      if(junf.eq.0)return                                                       !/*puy4c2     30*/
  110 if(mfl(lmfl+1,iap).eq.0)return                                            !/*puy4c2     31*/
      j=-mfl(lmfl+1,iap)                                                        !/*puy4c2     32*/
      if(j.gt.0)go to 120                                                       !/*puy4c2     33*/
      ju=mfl(lmfl+1,iap)                                                        !/*puy4c2     34*/
      call dunlev(iap)                                                          !/*puy4c2     35*/
      go to 90                                                                  !/*puy4c2     36*/
  120 do 130 i=1,3                                                              !/*puy4c2     37*/
  130 if(ijk(i).lt.laf(j+i,1).or.ijk(i).gt.laf(j+i,1)+laf(j+i,2)-1)             !/*puyx        8*/
     & go to 150                                                                !/*puy4c2     39*/
      n=3+iii-laf(j+1,1)+laf(j+1,2)*(jjj-laf(j+2,1)+laf(j+2,2)*                 !/*puy4c2     40*/
     & (kkk-laf(j+3,1)))                                                        !/*puy4c2     41*/
      if(laf(j+1,n).eq.0)go to 150                                              !/*puy4c2     42*/
      if(laf(j+1,n).eq.abs(jun(ljun+iap)))return                                !/*puy4c2     43*/
      ju=laf(j+1,n)                                                             !/*puy4c2     44*/
      call dunlev(iap)                                                          !/*puy4c2     45*/
      go to 90                                                                  !/*puy4c2     46*/
  140 continue                                                                  !/*puy4c2     47*/
  150 iap=mxa                                                                   !/*puy4c2     48*/
#endif                                                                          /*!puyx        9*/
      return                                                                    !/*puy4c2     49*/
      end                                                                       !/*puy4c2     50*/
