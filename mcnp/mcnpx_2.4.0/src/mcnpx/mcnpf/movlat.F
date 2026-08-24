!#ident movlat.F from deck ml                                                   !/*ml          1*/
      subroutine movlat(i1)                                                     !/*ml          2*/
!        move into the lattice element beyond surface lja(llja+i1).             !/*ml          3*/
      use global_data                                                           !/*mlx         1*/
!                                                                               !/*mlx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*mlx         3*/
      implicit integer(kindi) (i-n)                                             !/*mlx         4*/
!                                                                               !/*mlx         5*/
      dimension ij(8),kj(8,2)                                                   !/*mlx         6*/
      data ij/1,-1,1,-1,1,-1,1,-1/,kj/1,1,2,2,3,3,0,0,1,1,2,2,2,2,3,3/          !/*ml          7*/
!                                                                               !/*ml          8*/
      iap=icl                                                                   !/*ml          9*/
      n=i1-lca(llca+icl)+1                                                      !/*ml         10*/
      l=lat(llat+1,icl)                                                         !/*ml         11*/
      i=ij(n)                                                                   !/*ml         12*/
      jsu=abs(lja(llja+i1+i))                                                   !/*ml         13*/
      k=kj(n,l)                                                                 !/*ml         14*/
      m=lat(llat+2,icl)                                                         !/*ml         15*/
      xxx=xxx-i*vcl(lvcl+1,k,m)                                                 !/*ml         16*/
      yyy=yyy-i*vcl(lvcl+2,k,m)                                                 !/*ml         17*/
      zzz=zzz-i*vcl(lvcl+3,k,m)                                                 !/*ml         18*/
      ijk(k)=ijk(k)+i                                                           !/*mlx         7*/
      if(mfl(lmfl+1,icl).ge.0)go to 10                                          !/*ml         20*/
      j=-mfl(lmfl+1,icl)                                                        !/*ml         21*/
      if(ijk(k).lt.laf(j+k,1).or.ijk(k).ge.laf(j+k,1)+laf(j+k,2))               !/*mlx         8*/
     & go to 20                                                                 !/*mlx         9*/
   10 if(l.eq.1)return                                                          !/*ml         23*/
      if(n.ne.5.and.n.ne.6)return                                               !/*ml         24*/
      xxx=xxx+i*vcl(lvcl+1,1,m)                                                 !/*ml         25*/
      yyy=yyy+i*vcl(lvcl+2,1,m)                                                 !/*ml         26*/
      zzz=zzz+i*vcl(lvcl+3,1,m)                                                 !/*ml         27*/
      iii=iii-i                                                                 !/*ml         28*/
      if(mfl(lmfl+1,icl).ge.0)return                                            !/*ml         29*/
      j=-mfl(lmfl+1,icl)                                                        !/*ml         30*/
      if(iii.ge.laf(j+1,1).and.iii.lt.laf(j+1,1)+laf(j+1,2))return              !/*ml         31*/
   20 iap=-12345                                                                !/*ml4a        1*/
      return                                                                    !/*ml         33*/
      end                                                                       !/*ml         34*/
