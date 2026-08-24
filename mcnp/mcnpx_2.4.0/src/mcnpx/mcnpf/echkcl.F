!#ident echkcl.F from deck eh4b                                                 !/*eh4b        1*/
      subroutine echkcl(j)                                                      !/*eh4b        2*/
!        version of chkcel for charged particles. j has chkcel meaning.         !/*ehx         1*/
!        chkcel is called by electr and chg_pl because charged particles        !/*ehx         2*/
!        may be reflected back to present cell by angular deflection.           !/*ehx         3*/
!        for repeated structures, chkcel must be called at the level            !/*ehx         4*/
!        (levp) of the shortest distance to boundary.                           !/*ehx         5*/
      use global_data                                                           !/*ehx         6*/
!                                                                               !/*ehx         7*/
      implicit real(kindr) (a-h,o-z)                                            !/*ehx         8*/
      implicit integer(kindi) (i-n)                                             !/*ehx         9*/
!                                                                               !/*ehx        10*/
!                                                                               !/*eh4b        9*/
!        call regular chkcel for no repeated structures or lattices             !/*eh4b       10*/
!        or if lev=levp (shortest distance is in level lev).                    !/*eh4b       11*/
      if(junf.ne.0.and.lev.ne.levp)go to 10                                     !/*eh4b       12*/
      call  chkcel(icl,2,j)                                                     !/*eh4b       13*/
      return                                                                    !/*eh4b       14*/
!                                                                               !/*eh4b       15*/
!        in case of repeated structures, load present values in case we         !/*eh4b       16*/
!        stay in icl. then load closest values to find j.                       !/*eh4b       17*/
   10 le=lev                                                                    !/*eh4b       18*/
      xx=xxx                                                                    !/*eh4b       19*/
      yy=yyy                                                                    !/*eh4b       20*/
      zz=zzz                                                                    !/*eh4b       21*/
      uu=uuu                                                                    !/*eh4b       22*/
      vv=vvv                                                                    !/*eh4b       23*/
      ww=www                                                                    !/*eh4b       24*/
      ic=icl                                                                    !/*eh4b       25*/
      ii=iii                                                                    !/*eh4b       26*/
      jj=jjj                                                                    !/*eh4b       27*/
      kk=kkk                                                                    !/*eh4b       28*/
      lev=levp                                                                  !/*eh4b       29*/
      xxx=udt(1,lev)                                                            !/*eh4b       30*/
      yyy=udt(2,lev)                                                            !/*eh4b       31*/
      zzz=udt(3,lev)                                                            !/*eh4b       32*/
      uuu=udt(4,lev)                                                            !/*eh4b       33*/
      vvv=udt(5,lev)                                                            !/*eh4b       34*/
      www=udt(6,lev)                                                            !/*eh4b       35*/
      icl=udt(7,lev)                                                            !/*eh4b       36*/
      iii=udt(8,lev)                                                            !/*eh4b       37*/
      jjj=udt(9,lev)                                                            !/*eh4b       38*/
      kkk=udt(10,lev)                                                           !/*eh4b       39*/
      call chkcel(icl,2,j)                                                      !/*eh4b       40*/
!                                                                               !/*eh4b       41*/
!        reload kept values.                                                    !/*eh4b       42*/
      lev=le                                                                    !/*eh4b       43*/
      xxx=xx                                                                    !/*eh4b       44*/
      yyy=yy                                                                    !/*eh4b       45*/
      zzz=zz                                                                    !/*eh4b       46*/
      uuu=uu                                                                    !/*eh4b       47*/
      vvv=vv                                                                    !/*eh4b       48*/
      www=ww                                                                    !/*eh4b       49*/
      icl=ic                                                                    !/*eh4b       50*/
      iii=ii                                                                    !/*eh4b       51*/
      jjj=jj                                                                    !/*eh4b       52*/
      kkk=kk                                                                    !/*eh4b       53*/
      return                                                                    !/*eh4b       54*/
      end                                                                       !/*eh4b       55*/
