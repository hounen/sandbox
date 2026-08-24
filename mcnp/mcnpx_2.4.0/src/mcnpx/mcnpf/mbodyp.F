!#ident mbodyp.F from deck mbp4c2                                               !/*mbp4c2      1*/
      subroutine mbodyp(mm,ii,ij,ik,iy,j,jj,jn,ia,x,h,sc)                       !/*mbp4c2      2*/
!        provide planes for macrobodies.                                        !/*mbp4c2      3*/
!        mm=1 to 8 treated differently according to the call.                   !/*mbp4c2      4*/
      use global_data                                                           !/*mbpx        1*/
      use imcn_data                                                             !/*mbpx        2*/
!                                                                               !/*mbpx        3*/
      implicit real(kindr) (a-h,o-z)                                            !/*mbpx        4*/
      implicit integer(kindi) (i-n)                                             !/*mbpx        5*/
!                                                                               !/*mbpx        6*/
      dimension sc(33)                                                          !/*mbp4c2      7*/
      dimension ia(3)                                                           !/*mbp4c2      8*/
!                                                                               !/*mbp4c2      9*/
      x=-x                                                                      !/*mbp4c2     10*/
      if(mm.eq.4.and.j.ne.3)x=-x                                                !/*mbp4c2     11*/
      if(mm.ne.7.and.ij.eq.0)go to 30                                           !/*mbp4c2     12*/
      if(mm.eq.7.and.ia(jj).eq.0)go to 30                                       !/*mbp4c2     13*/
      if(mm.lt.4.and.j.eq.2.and.sc(3+ij).lt.0.)go to 10                         !/*mbp4c2     14*/
      if(mm.lt.4.and.j.eq.3.and.sc(3+ij).gt.0.)go to 10                         !/*mbp4c2     15*/
      if((mm.eq.4.or.mm.eq.5).and.x*sc(12+(jj-1)*3+ij).lt.0.)go to 10           !/*mbp4c2     16*/
      if(mm.eq.6.and.x.lt.0.)go to 10                                           !/*mbp4c2     17*/
      if(mm.eq.7.and.x*sc(15+jj*3+ia(jj)).lt.0.)go to 10                        !/*mbp4c2     18*/
      if(mm.eq.8.and.x.lt.0..and.sc(15+ij).gt.0.)go to 10                       !/*mbp4c2     19*/
      if(mm.eq.8.and.x.gt.0..and.sc(15+ij).lt.0.)go to 10                       !/*mbp4c2     20*/
!        set up a special plane for + plane                                     !/*mbp4c2     21*/
      if(mm.ne.7)kst(lkst+mxj+j)=1+ij                                           !/*mbp4c2     22*/
      if(mm.eq.7)kst(lkst+mxj+j)=1+ia(jj)                                       !/*mbp4c2     23*/
      if(mm.ne.6.and.mm.ne.7)scf(iy+1)=sc(ij)                                   !/*mbp4c2     24*/
      if(mm.lt.4.and.sc(3+ij).gt.0.)scf(iy+1)=sc(ij)+sc(3+ij)                   !/*mbp4c2     25*/
      if((mm.eq.4.or.mm.eq.5).and.x.gt.0.)                                      !/*mbp4c2     26*/
     & scf(iy+1)=sc(ij)+sc(12+(jj-1)*3+ij)*sc(21+jj)                            !/*mbp4c2     27*/
      if(mm.eq.6)scf(iy+1)=sc(j+1)                                              !/*mbp4c2     28*/
      if(mm.eq.6.and.jn.eq.4.and.ik-10.le.jj)scf(iy+1)=sc(j+3)                  !/*mbp4c2     29*/
      if(mm.eq.7)scf(iy+1)=sc(ia(jj))+x*sc(15+jj*3+ia(jj))*sc(30+jj)            !/*mbp4c2     30*/
      if(mm.eq.8.and.x.gt.0.)scf(iy+1)=sc(ij)+sc(15+ij)*h                       !/*mbp4c2     31*/
      iy=iy+1                                                                   !/*mbp4c2     32*/
      go to 50                                                                  !/*mbp4c2     33*/
!                                                                               !/*mbp4c2     34*/
!        set up a general plane for - plane to keep negative sense.             !/*mbp4c2     35*/
   10 do 20 k=1,4                                                               !/*mbp4c2     36*/
      scf(iy+k)=0.                                                              !/*mbp4c2     37*/
      if((mm.lt.6.or.mm.eq.8).and.k.le.3.and.ij.eq.k)scf(iy+k)=-1.              !/*mbp4c2     38*/
      if(mm.eq.6.and.k.le.3.and.jn.eq.6.and.jj.eq.k)scf(iy+k)=x                 !/*mbp4c2     39*/
      if(mm.eq.6.and.k.le.3.and.jn.eq.4.and.ik-10.gt.jj.and.jj.eq.k)            !/*mbp4c2     40*/
     & scf(iy+k)=x                                                              !/*mbp4c2     41*/
      if(mm.eq.6.and.k.le.3.and.jn.eq.4.and.ik-10.le.jj.and.jj+1.eq.k)          !/*mbp4c2     42*/
     & scf(iy+k)=x                                                              !/*mbp4c2     43*/
      if(mm.eq.7.and.k.le.3.and.ia(jj).eq.k)scf(iy+k)=-1.                       !/*mbp4c2     44*/
      if(k.ne.4)go to 20                                                        !/*mbp4c2     45*/
      if(mm.lt.4.and.sc(3+ij).gt.0.)scf(iy+k)=-sc(ij)                           !/*mbp4c2     46*/
      if(mm.lt.4.and.sc(3+ij).lt.0.)scf(iy+k)=-(sc(ij)+sc(ij+3))                !/*mbp4c2     47*/
      if((mm.eq.4.or.mm.eq.5.or.mm.eq.8).and.x.lt.0.)scf(iy+k)=-sc(ij)          !/*mbp4c2     48*/
      if((mm.eq.4.or.mm.eq.5).and.x.gt.0.)                                      !/*mbp4c2     49*/
     & scf(iy+k)=-(sc(ij)+sc(12+(jj-1)*3+ij)*sc(21+jj))                         !/*mbp4c2     50*/
      if(mm.eq.7)scf(iy+k)=-(sc(ia(jj))+x*sc(15+jj*3+ia(jj))*sc(30+jj))         !/*mbp4c2     51*/
      if(mm.eq.8.and.x.gt.0.)scf(iy+k)=-(sc(ij)+sc(15+ij)*h)                    !/*mbp4c2     52*/
      if(mm.ne.6)go to 20                                                       !/*mbp4c2     53*/
      scf(iy+k)=-sc(j-1)                                                        !/*mbp4c2     54*/
      if(jn.eq.4.and.ik-10.le.jj)scf(iy+k)=-sc(j+1)                             !/*mbp4c2     55*/
   20 continue                                                                  !/*mbp4c2     56*/
      iy=iy+4                                                                   !/*mbp4c2     57*/
      go to 50                                                                  !/*mbp4c2     58*/
!                                                                               !/*mbp4c2     59*/
!       off-axis plane.                                                         !/*mbp4c2     60*/
   30 do 40 k=1,4                                                               !/*mbp4c2     61*/
      if(mm.eq.1)scf(iy+k)=sc(3+k)*x/h                                          !/*mbp4c2     62*/
      if(mm.eq.2)scf(iy+k)=sc(8+k)*x                                            !/*mbp4c2     63*/
      if(mm.eq.3)scf(iy+k)=sc(7+k)*x                                            !/*mbp4c2     64*/
      if(mm.ge.4.and.mm.ne.7.and.mm.ne.8)scf(iy+k)=sc(12+(jj-1)*3+k)*x          !/*mbp4c2     65*/
      if(mm.eq.7)scf(iy+k)=sc(15+jj*3+k)*x                                      !/*mbp4c2     66*/
      if(mm.eq.8)scf(iy+k)=sc(15+k)*x                                           !/*mbp4c2     67*/
      if(k.ne.4)go to 40                                                        !/*mbp4c2     68*/
      scf(iy+k)=scf(iy+1)*sc(1)+scf(iy+2)*sc(2)+scf(iy+3)*sc(3)                 !/*mbp4c2     69*/
      if(mm.lt.4.and.j.eq.2)scf(iy+k)=scf(iy+k)+                                !/*mbp4c2     70*/
     & scf(iy+1)*sc(4)+scf(iy+2)*sc(5)+scf(iy+3)*sc(6)                          !/*mbp4c2     71*/
      if(mm.ge.4.and.mm.ne.7.and.mm.ne.8.and.x.gt.0.)                           !/*mbp4c2     72*/
     & scf(iy+k)=scf(iy+k)+(scf(iy+1)*sc(13+(jj-1)*3)+                          !/*mbp4c2     73*/
     & scf(iy+2)*sc(14+(jj-1)*3)+scf(iy+3)*sc(15+(jj-1)*3))*sc(21+jj)           !/*mbp4c2     74*/
      if(mm.eq.7)scf(iy+k)=scf(iy+k)+x*(scf(iy+1)*sc(16+jj*3)+                  !/*mbp4c2     75*/
     & scf(iy+2)*sc(17+jj*3)+scf(iy+3)*sc(18+jj*3))*sc(30+jj)                   !/*mbp4c2     76*/
      if(mm.eq.8.and.x.gt.0.)scf(iy+k)=scf(iy+k)+(scf(iy+1)*sc(16)+             !/*mbp4c2     77*/
     & scf(iy+2)*sc(17)+scf(iy+3)*sc(18))*h                                     !/*mbp4c2     78*/
   40 continue                                                                  !/*mbp4c2     79*/
      iy=iy+4                                                                   !/*mbp4c2     80*/
   50 return                                                                    !/*mbp4c2     81*/
      end                                                                       !/*mbp4c2     82*/
