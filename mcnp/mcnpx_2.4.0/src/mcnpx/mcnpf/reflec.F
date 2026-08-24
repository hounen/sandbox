!#ident reflec.F from deck re                                                   !/*re          1*/
      subroutine reflec                                                         !/*re          2*/
!        reflect the particle trajectory at surface jsu.                        !/*re          3*/
      use global_data                                                           !/*rex         1*/
!                                                                               !/*rex         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*rex         3*/
      implicit integer(kindi) (i-n)                                             !/*rex         4*/
!                                                                               !/*rex         5*/
!                                                                               !/*re          5*/
      l=lev                                                                     !/*re          6*/
      if(levp.eq.l)go to 20                                                     !/*re          7*/
      udt(1,l)=xxx                                                              !/*re          8*/
      udt(2,l)=yyy                                                              !/*re          9*/
      udt(3,l)=zzz                                                              !/*re         10*/
      lev=levp                                                                  !/*re         11*/
      do 10 i=1,6                                                               !/*re         12*/
   10 gpblcm(i)=udt(i,lev)                                                      !/*re         13*/
   20 cs=angl()                                                                 !/*re         14*/
!                                                                               !/*re4a        1*/
!        white boundary (cosine distribution scatter off surface).              !/*re4b        1*/
      if(ksu(lksu+jsu).ne.-2)go to 22                                           !/*re4a        3*/
      c=sign(sqrt(rang()),-cs)                                                  !/*re4b        3*/
      call rotas(c,ang(1),uuu,0,irt)                                            !/*re4b        4*/
      go to 24                                                                  !/*re4b        5*/
!                                                                               !/*re4a       10*/
!        reflecting boundary (specular reflection off surface).                 !/*re4a       11*/
   22 uuu=uuu-2.*cs*ang(1)                                                      !/*re4a       12*/
      vvv=vvv-2.*cs*ang(2)                                                      !/*re         16*/
      www=www-2.*cs*ang(3)                                                      !/*re         17*/
!                                                                               !/*re4b        9*/
!        transfer reflection to other levels.                                   !/*re4b       10*/
   24 if(lev.ne.0)call uplev(uuu)                                               !/*re4b       11*/
      if(lev.eq.l)go to 40                                                      !/*re         19*/
      do 30 lev=levp,l-1                                                        !/*re4b       12*/
      ic=udt(7,lev)                                                             !/*re         21*/
      udt(4,lev)=uuu                                                            !/*re         22*/
      udt(5,lev)=vvv                                                            !/*re         23*/
      udt(6,lev)=www                                                            !/*re         24*/
      j=-mfl(lmfl+1,ic)                                                         !/*re         25*/
      if(j.lt.0)m=mfl(lmfl+3,ic)                                                !/*re         26*/
      if(j.gt.0)m=laf(j+3,3+int(udt(8,lev))-laf(j+1,1)+laf(j+1,2)*              !/*re         27*/
     & (int(udt(9,lev))-laf(j+2,1)+laf(j+2,2)*                                  !/*re         28*/
     & (int(udt(10,lev))-laf(j+3,1))))                                          !/*re         29*/
      if(m.eq.0)go to 30                                                        !/*re         30*/
      uuu=udt(4,lev)*trf(ltrf+5,m)+udt(5,lev)*trf(ltrf+8,m)+                    !/*re         31*/
     & udt(6,lev)*trf(ltrf+11,m)                                                !/*re         32*/
      vvv=udt(4,lev)*trf(ltrf+6,m)+udt(5,lev)*trf(ltrf+9,m)+                    !/*re         33*/
     & udt(6,lev)*trf(ltrf+12,m)                                                !/*re         34*/
      www=udt(4,lev)*trf(ltrf+7,m)+udt(5,lev)*trf(ltrf+10,m)+                   !/*re         35*/
     & udt(6,lev)*trf(ltrf+13,m)                                                !/*re         36*/
   30 continue                                                                  !/*re         37*/
      lev=l                                                                     !/*re4b       13*/
      xxx=udt(1,l)                                                              !/*re         38*/
      yyy=udt(2,l)                                                              !/*re         39*/
      zzz=udt(3,l)                                                              !/*re         40*/
   40 if(lca(llca+icl).lt.0)call chkcel(icl,3,j)                                !/*re         41*/
      return                                                                    !/*re         42*/
      end                                                                       !/*re         43*/
