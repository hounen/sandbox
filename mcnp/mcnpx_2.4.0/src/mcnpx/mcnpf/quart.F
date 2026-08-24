!#ident quart.F from deck qt                                                    !/*qt          1*/
      subroutine quart(n,c,r4,jj,mm)                                            !/*qt          2*/
!        use the procedure of la-4299 to solve the quartic equation             !/*qt          3*/
!           x**4+b*x**3+c*x**2+d*x+e=0  (c(i),i=1,5)=1,b,c,d,e                  !/*qt          4*/
!        the jj real roots are returned in r4.  the roots with odd              !/*qt          5*/
!        multiplicity are listed first, in ascending order.  if mm=0,           !/*qt          6*/
!        the roots with even multiplicity are omitted.                          !/*qt          7*/
      use global_parameters                                                     !/*qtx         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*qtx         2*/
      implicit integer(kindi) (i-n)                                             !/*qtx         3*/
      dimension c(5),r4(4),tp(16)                                               !/*qt         10*/
      parameter (pf=(2.*3.1415926535898d0)/3.,th=1d0/3.)                        !/*qt         11*/
!                                                                               !/*qt         12*/
      jj=0                                                                      !/*qt         13*/
      if(n.eq.3)go to 90                                                        !/*qt         14*/
!                                                                               !/*qt         15*/
!        let y=x+b/4 to reduce the quartic equation to                          !/*qt         16*/
!          y**4+q*y**2+r*y+s=0  (tp(i),i=1,5)=b/2,b/4,q,r,s                     !/*qt         17*/
      tp(1)=.5*c(2)                                                             !/*qt         18*/
      tp(2)=.25*c(2)                                                            !/*qt         19*/
      tp(3)=c(3)-1.5*tp(1)**2                                                   !/*qt         20*/
      tp(4)=c(4)+tp(1)*(tp(1)**2-c(3))                                          !/*qt         21*/
      tp(5)=c(5)-.0625*tp(1)*(5.*c(4)-c(3)*tp(1)+3.*tp(4))                      !/*qt         22*/
!                                                                               !/*qt         23*/
!        steps 2-7 (la-4299) (tp(i),i=6,11)=b/3,c,d,p/3,q/2,w                   !/*qt         24*/
      if(abs(tp(4)).le.1.e-10*max(abs(c(4)),tp(1)**2,abs(tp(1)*c(3))))          !/*qt         25*/
     & go to 40                                                                 !/*qt         26*/
      tp(6)=2.*th*tp(3)                                                         !/*qt         27*/
      tp(7)=tp(3)**2-4.*tp(5)                                                   !/*qt         28*/
      tp(8)=-tp(4)**2                                                           !/*qt         29*/
      tp(9)=th*tp(7)-tp(6)**2                                                   !/*qt         30*/
      tp(10)=.5*(tp(8)-tp(6)*(tp(7)-2.*tp(6)**2))                               !/*qt         31*/
      tp(11)=tp(9)**3+tp(10)**2                                                 !/*qt         32*/
      if(abs(tp(11)).le.1.e-10*max(abs(tp(9)**3),tp(10)**2))go to 10            !/*qt         33*/
      if(tp(11).gt.0.)go to 30                                                  !/*qt         34*/
      tp(12)=sqrt(-tp(9))                                                       !/*qt         35*/
      if(tp(6).ge.0..or.tp(7).le.0.)return                                      !/*qt         36*/
      tp(13)=-tp(10)/tp(12)**3                                                  !/*qt         37*/
      if(abs(tp(13)).ge.1.)go to 10                                             !/*qt         38*/
      tp(13)=acos(tp(13))                                                       !/*qt         39*/
      tp(14)=2.*tp(12)*cos(th*tp(13))-tp(6)                                     !/*qt         40*/
      if(tp(14).lt.1.e-10*abs(tp(6)))go to 40                                   !/*qt         41*/
      tp(15)=.5*sqrt(tp(14))                                                    !/*qt         42*/
      tp(14)=-tp(14)-3.*tp(6)                                                   !/*qt         43*/
      tp(16)=tp(4)/tp(15)                                                       !/*qt         44*/
      if(abs(tp(16)).ge.tp(14))go to 10                                         !/*qt         45*/
      jj=4                                                                      !/*qt         46*/
      tp(13)=.5*sqrt(tp(14)+tp(16))                                             !/*qt         47*/
      r4(1)=-tp(13)-tp(15)-tp(2)                                                !/*qt         48*/
      r4(2)=tp(13)-tp(15)-tp(2)                                                 !/*qt         49*/
      tp(13)=.5*sqrt(tp(14)-tp(16))                                             !/*qt         50*/
      r4(3)=tp(15)-tp(13)-tp(2)                                                 !/*qt         51*/
      r4(4)=tp(15)+tp(13)-tp(2)                                                 !/*qt         52*/
      return                                                                    !/*qt         53*/
!                                                                               !/*qt         54*/
!        steps 8-11 (la-4299) double or triple roots.                           !/*qt         55*/
   10 tp(12)=sign(sqrt(abs(tp(9))),tp(10))                                      !/*qt         56*/
      tp(13)=-2.*tp(12)-tp(6)                                                   !/*qt         57*/
      tp(14)=tp(12)-tp(6)                                                       !/*qt         58*/
      tp(15)=sign(.5*sqrt(abs(tp(13))),tp(4))                                   !/*qt         59*/
      if(tp(14).le.0.)go to 20                                                  !/*qt         60*/
      tp(16)=sqrt(tp(14))                                                       !/*qt         61*/
      jj=2                                                                      !/*qt         62*/
      r4(1)=-tp(15)-tp(2)-tp(16)                                                !/*qt         63*/
      r4(2)=tp(16)-tp(15)-tp(2)                                                 !/*qt         64*/
      if(mm.eq.0)return                                                         !/*qt         65*/
      if(tp(10).eq.0.)return                                                    !/*qt         66*/
      jj=3                                                                      !/*qt         67*/
      r4(3)=tp(15)-tp(2)                                                        !/*qt         68*/
      return                                                                    !/*qt         69*/
   20 if(mm.eq.0)return                                                         !/*qt         70*/
      jj=1                                                                      !/*qt         71*/
      r4(1)=-tp(15)-tp(2)                                                       !/*qt         72*/
      return                                                                    !/*qt         73*/
!                                                                               !/*qt         74*/
!        step 12 (la-4299)                                                      !/*qt         75*/
   30 tp(12)=-tp(10)-sign(sqrt(tp(11)),tp(10))                                  !/*qt         76*/
      tp(12)=sign(abs(tp(12))**th,tp(12))                                       !/*qt         77*/
      tp(14)=tp(12)-tp(9)/tp(12)-tp(6)                                          !/*qt         78*/
      if(tp(14).lt.1.e-10*abs(tp(6)))go to 40                                   !/*qt         79*/
      tp(15)=.5*sign(sqrt(tp(14)),tp(4))                                        !/*qt         80*/
      tp(16)=abs(tp(4)/tp(15))-tp(14)-3.*tp(6)                                  !/*qt         81*/
      if(tp(16).le.0.)return                                                    !/*qt         82*/
      tp(16)=.5*sqrt(tp(16))                                                    !/*qt         83*/
      jj=2                                                                      !/*qt         84*/
      r4(1)=-tp(16)-tp(15)-tp(2)                                                !/*qt         85*/
      r4(2)=tp(16)-tp(15)-tp(2)                                                 !/*qt         86*/
      return                                                                    !/*qt         87*/
!                                                                               !/*qt         88*/
!        steps 13-24 (la-4299) trivial case; r=tp(4)=0                          !/*qt         89*/
   40 tp(6)=.5*tp(3)                                                            !/*qt         90*/
      tp(7)=tp(6)**2-tp(5)                                                      !/*qt         91*/
      tp(9)=1.e-8*max(tp(6)**2,abs(tp(5)))                                      !/*qt         92*/
      if(tp(7).le.tp(9))go to 60                                                !/*qt         93*/
      tp(8)=sqrt(tp(7))                                                         !/*qt         94*/
      tp(16)=tp(8)-tp(6)                                                        !/*qt         95*/
      if(tp(16).le.0.)go to 70                                                  !/*qt         96*/
      tp(15)=-tp(6)-tp(8)                                                       !/*qt         97*/
      tp(16)=sqrt(tp(16))                                                       !/*qt         98*/
      if(tp(15).gt.0.)go to 50                                                  !/*qt         99*/
      jj=2                                                                      !/*qt        100*/
      r4(1)=-tp(16)-tp(2)                                                       !/*qt        101*/
      r4(2)=tp(16)-tp(2)                                                        !/*qt        102*/
      if(mm.eq.0)return                                                         !/*qt        103*/
      if(tp(15).ne.0.)return                                                    !/*qt        104*/
      jj=3                                                                      !/*qt        105*/
      r4(3)=-tp(2)                                                              !/*qt        106*/
      return                                                                    !/*qt        107*/
   50 tp(15)=sqrt(tp(15))                                                       !/*qt        108*/
      jj=4                                                                      !/*qt        109*/
      r4(1)=-tp(16)-tp(2)                                                       !/*qt        110*/
      r4(2)=-tp(15)-tp(2)                                                       !/*qt        111*/
      r4(3)=tp(15)-tp(2)                                                        !/*qt        112*/
      r4(4)=tp(16)-tp(2)                                                        !/*qt        113*/
      return                                                                    !/*qt        114*/
   60 if(mm.eq.0)return                                                         !/*qt        115*/
      if(tp(7).lt.-tp(9).or.tp(6).gt.0.)return                                  !/*qt        116*/
      if(tp(6).eq.0.)go to 80                                                   !/*qt        117*/
      jj=2                                                                      !/*qt        118*/
      tp(16)=sqrt(-tp(6))                                                       !/*qt        119*/
      r4(1)=tp(16)-tp(2)                                                        !/*qt        120*/
      r4(2)=-tp(16)-tp(2)                                                       !/*qt        121*/
      return                                                                    !/*qt        122*/
   70 if(mm.eq.0)return                                                         !/*qt        123*/
      if(tp(16).lt.0.)return                                                    !/*qt        124*/
   80 jj=1                                                                      !/*qt        125*/
      r4(1)=-tp(2)                                                              !/*qt        126*/
      return                                                                    !/*qt        127*/
!                                                                               !/*qt        128*/
!        cubic equation.  x**3+b*x**2+c*x+d=0  (c(i),i=1,4)=1,b,c,d             !/*qt        129*/
!        let y=x+b/3.  then  y**3+p*y+q=0   (tp(i),i=1,4)=b/3,p/3,q/2,w         !/*qt        130*/
   90 tp(1)=th*c(2)                                                             !/*qt        131*/
      tp(2)=th*c(3)-tp(1)**2                                                    !/*qt        132*/
      tp(3)=.5*(c(4)-tp(1)*(c(3)-2.*tp(1)**2))                                  !/*qt        133*/
      tp(4)=tp(2)**3+tp(3)**2                                                   !/*qt        134*/
      if(tp(4).gt.0.)go to 110                                                  !/*qt        135*/
      if(tp(4).eq.0.)go to 100                                                  !/*qt        136*/
!                                                                               !/*qt        137*/
!        case i (la-4299)  3 distinct roots                                     !/*qt        138*/
      tp(5)=2.*sqrt(-tp(2))                                                     !/*qt        139*/
      tp(6)=8.*tp(3)/tp(5)**3                                                   !/*qt        140*/
      if(abs(tp(6)).ge.1.)go to 100                                             !/*qt        141*/
      tp(7)=th*acos(-tp(6))                                                     !/*qt        142*/
      r4(1)=tp(5)*cos(pf+tp(7))-tp(1)                                           !/*qt        143*/
      r4(2)=tp(5)*cos(pf-tp(7))-tp(1)                                           !/*qt        144*/
      r4(3)=tp(5)*cos(tp(7))-tp(1)                                              !/*qt        145*/
      jj=3                                                                      !/*qt        146*/
      return                                                                    !/*qt        147*/
!                                                                               !/*qt        148*/
!        case ii (la-4299) 1 distinct and 1 double root                         !/*qt        149*/
  100 r4(1)=-2.*sign(sqrt(-tp(2)),tp(3))-tp(1)                                  !/*qt        150*/
      jj=1                                                                      !/*qt        151*/
      if(mm.eq.0)return                                                         !/*qt        152*/
      if(r4(1).eq.0.)return                                                     !/*qt        153*/
      jj=2                                                                      !/*qt        154*/
      r4(2)=-.5*r4(1)                                                           !/*qt        155*/
      return                                                                    !/*qt        156*/
!                                                                               !/*qt        157*/
!        case iii (la-4299)  1 distinct root                                    !/*qt        158*/
  110 tp(5)=-tp(3)-sign(sqrt(tp(4)),tp(3))                                      !/*qt        159*/
      tp(5)=sign(abs(tp(5))**th,tp(5))                                          !/*qt        160*/
      r4(1)=tp(5)-tp(2)/tp(5)-tp(1)                                             !/*qt        161*/
      jj=1                                                                      !/*qt        162*/
      return                                                                    !/*qt        163*/
      end                                                                       !/*qt        164*/
