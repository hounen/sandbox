!#ident torus.F from deck to                                                    !/*to          1*/
      subroutine torus(ih,i,j,d1)                                               !/*to          2*/
!        calculate the intersections of the track with a torus.                 !/*to          3*/
      use global_data                                                           !/*tox         1*/
!                                                                               !/*tox         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*tox         3*/
      implicit integer(kindi) (i-n)                                             !/*tox         4*/
!                                                                               !/*tox         5*/
      dimension uu(5),xx(5)                                                     !/*to          5*/
!                                                                               !/*to          6*/
      ix=kst(lkst+j)-23                                                         !/*to4a        1*/
      uu(1)=uuu                                                                 !/*to          8*/
      uu(4)=uuu                                                                 !/*to          9*/
      uu(2)=vvv                                                                 !/*to         10*/
      uu(5)=vvv                                                                 !/*to         11*/
      uu(3)=www                                                                 !/*to         12*/
      xx(1)=xxx-scf(i)                                                          !/*to         13*/
      xx(4)=xx(1)                                                               !/*to         14*/
      xx(2)=yyy-scf(i+1)                                                        !/*to         15*/
      xx(5)=xx(2)                                                               !/*to         16*/
      xx(3)=zzz-scf(i+2)                                                        !/*to         17*/
      tpp(1)=uu(ix)**2                                                          !/*to         18*/
      tpp(2)=1.-tpp(1)                                                          !/*to         19*/
      tpp(3)=1./(tpp(2)+tpp(1)*scf(i+6))                                        !/*to         20*/
      tpp(4)=(2.*scf(i+3)*tpp(3))**2                                            !/*to         21*/
      tpp(5)=2.*(uu(ix+2)*xx(ix+2)+uu(ix+1)*xx(ix+1))                           !/*to         22*/
      tpp(6)=xx(ix+2)**2+xx(ix+1)**2                                            !/*to         23*/
      t1=(tpp(5)+2.*uu(ix)*xx(ix)*scf(i+6))*tpp(3)                              !/*to         24*/
      t2=(tpp(6)+scf(i+6)*xx(ix)**2+scf(i+3)**2-scf(i+5)**2)*tpp(3)             !/*to         25*/
!                                                                               !/*to         26*/
!        set up the quartic coefficients.  (uu(i),i=1,5)=1,b,c,d,e              !/*to         27*/
      uu(1)=1.                                                                  !/*to         28*/
      uu(2)=2.*t1                                                               !/*to         29*/
      uu(3)=t1**2+2.*t2-tpp(4)*tpp(2)                                           !/*to         30*/
      uu(4)=uu(2)*t2-tpp(4)*tpp(5)                                              !/*to         31*/
      uu(5)=t2**2-tpp(4)*tpp(6)                                                 !/*to         32*/
!                                                                               !/*to         33*/
!        eliminate the smallest root if the particle is sitting on              !/*to         34*/
!        the torus.  solve cubic instead of quartic.                            !/*to         35*/
      n=4                                                                       !/*to         36*/
      if(jsu.eq.j)n=3                                                           !/*to         37*/
!                                                                               !/*to         38*/
!        there are no positive roots if no coefficients are negative.           !/*to         39*/
      do 10 m=2,n                                                               !/*to         40*/
   10 if(uu(m).lt.0.)go to 20                                                   !/*to         41*/
      if(uu(n+1).ge.0.)return                                                   !/*to         42*/
!                                                                               !/*to         43*/
!        solve the quartic or cubic equation for the intersections.             !/*to         44*/
   20 call quart(n,uu,xx,jj,0)                                                  !/*to         45*/
      if(jj.eq.0)return                                                         !/*to         46*/
!                                                                               !/*to         47*/
!        zero in on legitimate positive roots.                                  !/*to         48*/
      do 60 m=1,jj                                                              !/*to         49*/
!        eliminate negative roots.                                              !/*to         50*/
      if(xx(m).lt.0.)go to 60                                                   !/*to         51*/
!        eliminate roots on the wrong branch of a degenerate torus.             !/*to         52*/
      if((t2+xx(m)*(t1+xx(m)))*scf(i+3).lt.0.)go to 60                          !/*to         53*/
      z0=1.e-8*(1.+abs(xx(m)))                                                  !/*to         54*/
      do 30 nr=1,20                                                             !/*to         55*/
      t3=uu(4)+xx(m)*(2.*uu(3)+xx(m)*(3.*uu(2)+4.*xx(m)))                       !/*to         56*/
      if(t3.eq.0.)go to 35                                                      !/*to4a        2*/
      t3=(uu(5)+xx(m)*(uu(4)+xx(m)*(uu(3)+xx(m)*(uu(2)+xx(m)))))/t3             !/*to         58*/
      xx(m)=xx(m)-t3                                                            !/*to         59*/
   30 if(abs(t3).le.z0)go to 40                                                 !/*to         60*/
   35 if(krflg.eq.2)call errprn(1,-1,0,zero,zero,' ',' ',                       !/*to4c2       1*/
     & 'bad quartic solver root.')                                              !/*to4c2       2*/
      go to 60                                                                  !/*to4a        6*/
!                                                                               !/*to         63*/
!        return with first (smallest) legitimate root if cell is simple.        !/*to         64*/
   40 if(lca(llca+ih).lt.0)go to 50                                             !/*to         65*/
      d1=xx(m)                                                                  !/*to         66*/
      return                                                                    !/*to         67*/
!                                                                               !/*to         68*/
!        put the roots in dti if the cell is complicated.                       !/*to         69*/
   50 nlt=nlt+1                                                                 !/*to         70*/
      dti(nlt)=xx(m)                                                            !/*to         71*/
      iti(nlt)=j                                                                !/*to         72*/
   60 continue                                                                  !/*to         73*/
      return                                                                    !/*to         74*/
      end                                                                       !/*to         75*/
