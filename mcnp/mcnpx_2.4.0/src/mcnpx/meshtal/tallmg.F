!     $Id: tallmg.F,v 1.6 2002/06/14 17:05:56 jxh Exp $                         !/*ta          1*/
      subroutine tallmg(ld,ng,d2,ar)                                            !/*ta          2*/
#ifdef MESHTAL                                                                  /*!ta          3*/
!       Determine the tally point in the detector grid, the distance,           !/*ta          4*/
!       and the direction cosines of the trace.                                 !/*ta          5*/
!       written by E. C. Snow 1996. revised 06/09/2000                          !/*ta          6*/
      use global_data                                                           !/*taxc        1*/
!                                                                               !/*taxc        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*taxc        3*/
      implicit integer(kindi) (i-n)                                             !/*taxc        4*/
!                                                                               !/*taxc        5*/
!                                                                               !/*ta          8*/
!       Determine the tally bins ibs and ibc in the s and t                     !/*ta          9*/
!       directions of the grid.                                                 !/*ta         10*/
      mt=iptal(lipt+6,4,ital)-1                                                 !/*ta         11*/
      ms=iptal(lipt+4,4,ital)-1                                                 !/*ta         12*/
      if(ms*mt.gt.0)go to 10                                                    !/*ta         13*/
      call expirx(0,'tallmg',                                                   !/*ta         15*/
     & 'The detector grid is not defined properly.')                            !/*ta         16*/
      return                                                                    !/*ta         17*/
!                                                                               !/*ta         18*/
   10 ls=iptal(lipt+4,1,ital)                                                   !/*ta         19*/
      lt=iptal(lipt+6,1,ital)                                                   !/*ta         20*/
      if(jptal(ljpt+8,ital).eq.3)go to 200                                      !/*ta         21*/
!                                                                               !/*ta         22*/
      ibs=1+int((ng-1)/mt)                                                      !/*ta         23*/
      ibc=ng-int((ng-1)/mt)*mt                                                  !/*ta         24*/
      if(jptal(ljpt+8,ital).eq.5)go to 100                                      !/*ta         25*/
!                                                                               !/*ta         26*/
!       Determine s and t for a rectangular grid.                               !/*ta         27*/
      if(ng.gt.1)go to 20                                                       !/*ta         28*/
      deltas=.5                                                                 !/*ta         29*/
      deltat=.5                                                                 !/*ta         30*/
      if(tds(ld+18).eq.0)go to 20                                               !/*ta         31*/
      deltas=rang()                                                             !/*ta         32*/
      deltat=rang()                                                             !/*ta         33*/
   20 ds=(tds(ls+ibs)-tds(ls+ibs-1))*deltas                                     !/*ta         34*/
      dt=(tds(lt+ibc)-tds(lt+ibc-1))*deltat                                     !/*ta         35*/
      s=tds(ls+ibs-1)+ds                                                        !/*ta         36*/
      t=tds(lt+ibc-1)+dt                                                        !/*ta         37*/
      rt=sqrt(s**2+t**2)                                                        !/*ta         38*/
      rp=tds(ld+16)                                                             !/*ta         39*/
      if(rt.gt.rp)go to 300                                                     !/*ta         40*/
!                                                                               !/*ta         41*/
      do 50 i=1,3                                                               !/*ta         42*/
      n=i+3                                                                     !/*ta         43*/
      l=i+9                                                                     !/*ta         44*/
      k=i+12                                                                    !/*ta         45*/
   50 tpp(n)=tds(ld+i)+t*tds(ld+l)+s*tds(ld+k)                                  !/*ta         46*/
      if(ipsc.eq.11)go to 70                                                    !/*ta         47*/
!                                                                               !/*ta         48*/
   52 if(lev.ne.0)go to 55                                                      !/*ta         49*/
      do 54 i=1,3                                                               !/*ta         50*/
   54 tpp(i+6)=gpblcm(i)                                                        !/*ta         51*/
      go to 58                                                                  !/*ta         52*/
   55 do 57 i=1,3                                                               !/*ta         53*/
   57 tpp(i+6)=udt(i,0)                                                         !/*ta         54*/
!                                                                               !/*ta         55*/
   58 do 60 i=1,3                                                               !/*ta         56*/
   60 tpp(i)=tpp(i+3)-tpp(i+6)                                                  !/*ta         57*/
      ddet=sqrt(tpp(1)**2+tpp(2)**2+tpp(3)**2)                                  !/*ta         58*/
      t1=1./ddet                                                                !/*ta         59*/
      uuu=tpp(1)*t1                                                             !/*ta         60*/
      vvv=tpp(2)*t1                                                             !/*ta         61*/
      www=tpp(3)*t1                                                             !/*ta         62*/
      if(lev.ne.0)call dddlev                                                   !/*ta         63*/
      return                                                                    !/*ta         64*/
!                                                                               !/*ta         65*/
!       Calculate the distance from a source plane to the point on              !/*ta         66*/
!       the detector grid for a contribution from a monodirectional             !/*ta         67*/
!       source particle.                                                        !/*ta         68*/
   70 d=uuu*(tpp(4)-sff(1,6))+vvv*(tpp(5)-sff(2,6))+                            !/*ta         69*/
     & www*(tpp(6)-sff(3,6))                                                    !/*ta         70*/
      if(d.lt.0.)go to 90                                                       !/*ta         71*/
      ddet=d                                                                    !/*ta         72*/
!                                                                               !/*ta         73*/
!       Move source particle back toward source from grid point                 !/*ta         74*/
!       parallel to the direction of the source.                                !/*ta         75*/
      xxx=tpp(4)-uuu*ddet                                                       !/*ta         76*/
      yyy=tpp(5)-vvv*ddet                                                       !/*ta         77*/
      zzz=tpp(6)-www*ddet                                                       !/*ta         78*/
!                                                                               !/*ta         79*/
!      Check that point is still on source disk.                                !/*ta         80*/
      r=(xxx-sff(1,6))**2+(yyy-sff(2,6))**2+(zzz-sff(3,6))**2                   !/*ta         81*/
      ic=abs(isic(10))                                                          !/*ta         82*/
      r1=sqq(lsqq+6,ic)                                                         !/*ta         83*/
      r2=sqq(lsqq+7,ic)                                                         !/*ta         84*/
      if(r.gt.r2**2.or.r.lt.r1**2)go to 90                                      !/*ta         85*/
!                                                                               !/*ta         86*/
!       Check that the point is still in the cookie-cutter cell.                !/*ta         87*/
      jsu=sff(1,2)                                                              !/*ta         88*/
      if(sff(1,14).eq.0)go to 80                                                !/*ta         89*/
      call chkcel(int(sff(1,14)),0,j)                                           !/*ta         90*/
      if(j.ne.0)go to 90                                                        !/*ta         91*/
!                                                                               !/*ta         92*/
   80 wgt=wgt/sff(1,16)                                                         !/*ta         93*/
      return                                                                    !/*ta         94*/
!                                                                               !/*ta         95*/
   90 wgt=0.                                                                    !/*ta         96*/
      return                                                                    !/*ta         97*/
!                                                                               !/*ta         98*/
!       Determine s and t for a cylindrical grid.                               !/*ta         99*/
  100 if(ng.gt.1)go to 110                                                      !/*ta        100*/
      deltas=.5                                                                 !/*ta        101*/
      deltat=.5                                                                 !/*ta        102*/
      if(tds(ld+18).eq.0)go to 110                                              !/*ta        103*/
      deltas=rang()                                                             !/*ta        104*/
      deltat=rang()                                                             !/*ta        105*/
  110 ds=(tds(ls+ibs)-tds(ls+ibs-1))*deltas                                     !/*ta        106*/
      dt=(tds(lt+ibc)-tds(lt+ibc-1))*deltat                                     !/*ta        107*/
      ct=(tds(lt+ibc-1)+dt)*pie/180.0                                           !/*ta        108*/
      rp=tds(ld+16)                                                             !/*ta        109*/
      s=rp*sin(ct)                                                              !/*ta        110*/
      t=rp*cos(ct)                                                              !/*ta        111*/
!                                                                               !/*ta        112*/
!       Find x, y, and z at center of origin of cylinder.                       !/*ta        113*/
      do 120 i=1,3                                                              !/*ta        114*/
      n=i+3                                                                     !/*ta        115*/
      l=i+9                                                                     !/*ta        116*/
      k=i+12                                                                    !/*ta        117*/
  120 tpp(n)=tds(ld+i)+t*tds(ld+l)+s*tds(ld+k)                                  !/*ta        118*/
!                                                                               !/*ta        119*/
!       Find x, y, and z with offset parallel to cylinder axis.                 !/*ta        120*/
      dl=tds(ls+ibs-1)+ds                                                       !/*ta        121*/
      do 130 i=1,3                                                              !/*ta        122*/
      n=i+3                                                                     !/*ta        123*/
      l=i+6                                                                     !/*ta        124*/
  130 tpp(n)=tpp(n)+dl*tds(ld+l)                                                !/*ta        125*/
!                                                                               !/*ta        126*/
!       Find distance from point to detector and direction cosines.             !/*ta        127*/
      go to 52                                                                  !/*ta        128*/
!                                                                               !/*ta        129*/
!       Set up for pinhole image detector.  Determine grid bins and             !/*ta        130*/
!       field-of-view if applicable.                                            !/*ta        131*/
!                                                                               !/*ta        132*/
  200 if(ipsc.eq.11)go to 280                                                   !/*ta        133*/
!                                                                               !/*ta        134*/
!       Get uuu,vvv,www and distance ddet to pinhole.                           !/*ta        135*/
      do 210 i=1,3                                                              !/*ta        136*/
  210 tpp(i)=tds(ld+i)-gpblcm(i)                                                !/*ta        137*/
      ddet=sqrt(tpp(1)**2+tpp(2)**2+tpp(3)**2)                                  !/*ta        138*/
      if(ddet.eq.0.)go to 290                                                   !/*ta        139*/
      t1=1./ddet                                                                !/*ta        140*/
      uuu=tpp(1)*t1                                                             !/*ta        141*/
      vvv=tpp(2)*t1                                                             !/*ta        142*/
      www=tpp(3)*t1                                                             !/*ta        143*/
!                                                                               !/*ta        144*/
!       Check field of view for collimated detector.                            !/*ta        145*/
      cs=tds(ld+7)*uuu+tds(ld+8)*vvv+tds(ld+9)*www                              !/*ta        146*/
      if(cs.le.0.)go to 300                                                     !/*ta        147*/
      if(tds(ld+17).eq.0.)go to 220                                             !/*ta        148*/
      r=sqrt(1-cs**2)*ddet                                                      !/*ta        149*/
      if(r.gt.tds(ld+17))go to 300                                              !/*ta        150*/
!                                                                               !/*ta        151*/
!       Check for finite pinhole.                                               !/*ta        152*/
  220 if(tds(ld+16).eq.0.)go to 230                                             !/*ta        153*/
  225 t1=2*rang()-1.                                                            !/*ta        154*/
      t2=2*rang()-1.                                                            !/*ta        155*/
      t3=sqrt(t1**2+t2**2)                                                      !/*ta        156*/
      if(t3.gt.1)go to 225                                                      !/*ta        157*/
      s=t1*tds(ld+16)                                                           !/*ta        158*/
      t=t2*tds(ld+16)                                                           !/*ta        159*/
      tpp(1)=tds(ld+1)+t*tds(ld+10)+s*tds(ld+13)-xxx                            !/*ta        160*/
      tpp(2)=tds(ld+2)+t*tds(ld+11)+s*tds(ld+14)-yyy                            !/*ta        161*/
      tpp(3)=tds(ld+3)+t*tds(ld+12)+s*tds(ld+15)-zzz                            !/*ta        162*/
      ddet=sqrt(tpp(1)**2+tpp(2)**2+tpp(3)**2)                                  !/*ta        163*/
      t1=1./ddet                                                                !/*ta        164*/
      uuu=tpp(1)*t1                                                             !/*ta        165*/
      vvv=tpp(2)*t1                                                             !/*ta        166*/
      www=tpp(3)*t1                                                             !/*ta        167*/
      cs=tds(ld+7)*uuu+tds(ld+8)*vvv+tds(ld+9)*www                              !/*ta        168*/
!                                                                               !/*ta        169*/
!       Define distance from pinhole to point on the grid.                      !/*ta        170*/
  230 d2=tds(ld+18)/cs                                                          !/*ta        171*/
      ddet=ddet+d2                                                              !/*ta        172*/
!                                                                               !/*ta        173*/
!       Determine x,y,z of point where ray intersects the grid.                 !/*ta        174*/
      x=xxx+ddet*uuu                                                            !/*ta        175*/
      y=yyy+ddet*vvv                                                            !/*ta        176*/
      z=zzz+ddet*www                                                            !/*ta        177*/
!                                                                               !/*ta        178*/
!       Get t and s values for x,y,z.                                           !/*ta        179*/
      t=tds(ld+10)*(x-tds(ld+4))+tds(ld+11)*(y-tds(ld+5))+                      !/*ta        180*/
     & tds(ld+12)*(z-tds(ld+6))                                                 !/*ta        181*/
      s=tds(ld+13)*(x-tds(ld+4))+tds(ld+14)*(y-tds(ld+5))+                      !/*ta        182*/
     & tds(ld+15)*(z-tds(ld+6))                                                 !/*ta        183*/
!                                                                               !/*ta        184*/
!       Loop over t and s bins and set ibc and ibs.                             !/*ta        185*/
      if(s.lt.tds(ls))go to 300                                                 !/*ta        186*/
      do 240 ibs=1,ms                                                           !/*ta        187*/
  240 if(s.le.tds(ls+ibs))go to 250                                             !/*ta        188*/
      go to 300                                                                 !/*ta        189*/
  250 if(t.lt.tds(lt))go to 300                                                 !/*ta        190*/
      do 260 ibc=1,mt                                                           !/*ta        191*/
  260 if(t.lt.tds(lt+ibc))go to 270                                             !/*ta        192*/
      go to 300                                                                 !/*ta        193*/
!                                                                               !/*ta        194*/
!       Determine the area of this grid bin and store in ar.                    !/*ta        195*/
  270 a=(tds(ls+ibs)-tds(ls+ibs-1))*(tds(lt+ibc)-tds(lt+ibc-1))                 !/*ta        196*/
      if(a.ne.0.)ar=abs(a)                                                      !/*ta        197*/
      return                                                                    !/*ta        198*/
!                                                                               !/*ta        199*/
  280 if(monod.ne.0)go to 300                                                   !/*ta        200*/
      call errprn(1,monod,0,zero,zero,' ',' ',                                  !/*taxk        1*/
     & '52hno contribution to grid from monodirectional source.')               !/*ta        203*/
      go to 300                                                                 !/*ta        206*/
!                                                                               !/*ta        207*/
!       Error in pinhole definition.                                            !/*ta        208*/
  290 continue                                                                  !/*ta        209*/
      call expirx(0,'tallmg','Error in pinhole definition.')                    !/*ta        211*/
      return                                                                    !/*ta        212*/
!                                                                               !/*ta        213*/
!       Set weight to zero and return.                                          !/*ta        214*/
  300 wgt=0.                                                                    !/*ta        215*/
!                                                                               !/*ta        216*/
#endif                                                                          /*!ta        217*/
      return                                                                    !/*ta        218*/
      end                                                                       !/*ta        219*/
