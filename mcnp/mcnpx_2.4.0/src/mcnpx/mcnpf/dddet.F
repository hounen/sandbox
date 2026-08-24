!#ident dddet.F from deck dd                                                    !/*dd          1*/
      subroutine dddet(ld)                                                      !/*dd          2*/
!        determine the direction and the distance to a detector.                !/*dd          3*/
!        ld points to the description of the detector in tds.                   !/*dd          4*/
      use global_data                                                           !/*ddx         1*/
!                                                                               !/*ddx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*ddx         3*/
      implicit integer(kindi) (i-n)                                             !/*ddx         4*/
!                                                                               !/*ddx         5*/
!                                                                               !/*dd          6*/
      if(lev.ne.0)go to 10                                                      !/*dd          7*/
      tpp(1)=xxx                                                                !/*dd          8*/
      tpp(2)=yyy                                                                !/*dd          9*/
      tpp(3)=zzz                                                                !/*dd         10*/
      go to 20                                                                  !/*dd         11*/
   10 tpp(1)=udt(1,0)                                                           !/*dd         12*/
      tpp(2)=udt(2,0)                                                           !/*dd         13*/
      tpp(3)=udt(3,0)                                                           !/*dd         14*/
!                                                                               !/*dd         15*/
!        point detector.                                                        !/*dd         16*/
   20 if(jptal(ljpt+8,ital).eq.2)go to 30                                       !/*dd         17*/
      ddet=sqrt((tds(ld+1)-tpp(1))**2+(tds(ld+2)-tpp(2))**2+                    !/*dd         18*/
     & (tds(ld+3)-tpp(3))**2)                                                   !/*dd         19*/
      t1=1./ddet                                                                !/*dd         20*/
      uuu=(tds(ld+1)-tpp(1))*t1                                                 !/*dd         21*/
      vvv=(tds(ld+2)-tpp(2))*t1                                                 !/*dd         22*/
      www=(tds(ld+3)-tpp(3))*t1                                                 !/*dd         23*/
      if(lev.ne.0)call dddlev                                                   !/*dd         24*/
      return                                                                    !/*dd         25*/
!                                                                               !/*dd         26*/
!        ring detector.                                                         !/*dd         27*/
   30 i=tds(ld+3)                                                               !/*dd         28*/
      tpp(4)=tpp(1)                                                             !/*dd         29*/
      tpp(5)=tpp(2)                                                             !/*dd         30*/
      t5=tpp(i+1)**2+tpp(i+2)**2                                                !/*dd         31*/
      tpp(8)=tds(ld+1)-tpp(i)                                                   !/*dd         32*/
      t1=tds(ld+2)**2+t5+tpp(8)**2                                              !/*dd         33*/
      t2=-2.*tds(ld+2)*tpp(i+2)                                                 !/*dd         34*/
      t3=-2.*tds(ld+2)*tpp(i+1)                                                 !/*dd         35*/
      if(t5.gt.0.)t1=min(t1,1.5*sqrt(t2**2+t3**2))                              !/*dd         36*/
      t4=sqrt(t1**2-(t2**2+t3**2))                                              !/*dd         37*/
      t5=t4*tan(pie*(rang()-.5))-t3                                             !/*dd         38*/
      t6=t1-t2                                                                  !/*dd         39*/
      t7=1./(t5**2+t6**2)                                                       !/*dd         40*/
!ccc     extra parenthesis required for convex compiler optimization.           !/*dd4b        1*/
      t8=(2.*t5)*(t6*t7)                                                        !/*dd4b        2*/
      t9=(t6**2-t5**2)*t7                                                       !/*dd         42*/
      wgt=wgt*(t1+t2*t9+t3*t8)/t4                                               !/*dd         43*/
      tpp(6)=tds(ld+2)*t8-tpp(i+1)                                              !/*dd         44*/
      tpp(9)=tpp(6)                                                             !/*dd         45*/
      tpp(7)=tds(ld+2)*t9-tpp(i+2)                                              !/*dd         46*/
      tpp(10)=tpp(7)                                                            !/*dd         47*/
      ddet=sqrt(tpp(6)**2+tpp(7)**2+tpp(8)**2)                                  !/*dd         48*/
      t1=1./ddet                                                                !/*dd         49*/
      uuu=tpp(9-i)*t1                                                           !/*dd         50*/
      vvv=tpp(10-i)*t1                                                          !/*dd         51*/
      www=tpp(11-i)*t1                                                          !/*dd         52*/
      if(lev.ne.0)call dddlev                                                   !/*dd         53*/
      return                                                                    !/*dd         54*/
      end                                                                       !/*dd         55*/
