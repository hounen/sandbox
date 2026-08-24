!#ident chksrc.F from deck ck                                                   !/*ck          1*/
      subroutine chksrc                                                         !/*ck          2*/
!        check consistency of icl,jsu with uuu,vvv,www,xxx,yyy,zzz.             !/*ck          3*/
!        this subroutine would not be needed if users never erred.              !/*ck          4*/
      use global_data                                                           !/*ckx         1*/
!                                                                               !/*ckx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*ckx         3*/
      implicit integer(kindi) (i-n)                                             !/*ckx         4*/
!                                                                               !/*ckx         5*/
!                                                                               !/*ck          6*/
!        check that icl is a valid cell name.                                   !/*ck          7*/
      iq=namchg(1,icl)                                                          !/*ck          8*/
      if(icl.ge.1.and.icl.le.mxa)go to 20                                       !/*ck          9*/
#ifdef MULTT                                                                    /*!ckxk        1*/
      call thread_lockon(jlock)                                                 !/*ckxk        2*/
#endif                                                                          /*!ckxk        3*/
      if(iq.ne.0)write(iuo,10)                                                  !/*ck         11*/
   10 format(/51h perhaps the source subroutine set icl equal to the,           !/*ck         12*/
     & 13h problem name/39h rather than to the program name of the,             !/*ck         13*/
     & 13h source cell.)                                                        !/*ck         14*/
      write(iuo,'(6h icl =,i9)')icl                                             !/*ck         15*/
      call expirx(1,'chksrc',                                                   !/*ck         16*/
     & 'icl set by the source is not a valid program name of a cell.')          !/*ck         17*/
      return                                                                    !/*ck         18*/
!                                                                               !/*ck         19*/
!        if jsu=0, check that xxx,yyy,zzz is in cell icl.                       !/*ck         20*/
   20 if(jsu.ne.0)go to 40                                                      !/*ck         21*/
      call chkcel(icl,0,j)                                                      !/*ck         22*/
      if(j.eq.0)go to 100                                                       !/*ck         23*/
      if(iq.ne.0)call chkcel(iq,0,j)                                            !/*ck         24*/
#ifdef MULTT                                                                    /*!ckxk        4*/
      call thread_lockon(jlock)                                                 !/*ckxk        5*/
#endif                                                                          /*!ckxk        6*/
      if(iq.ne.0.and.j.eq.0)write(iuo,10)                                       !/*ck         26*/
      write(iuo,30)xxx,yyy,zzz,icl,ncl(lncl+icl)                                !/*ck         27*/
   30 format(/6h xxx =,1pe12.5,4x,5hyyy =,e12.5,4x,5hzzz =,e12.5/               !/*ck         28*/
     & 13h cell:  icl =,i4,4x,14hproblem name =,i5)                             !/*ck         29*/
      call expirx(1,'chksrc',                                                   !/*ck         30*/
     & 'xxx,yyy,zzz and icl set by the source are inconsistent.')               !/*ck         31*/
      return                                                                    !/*ck         32*/
!                                                                               !/*ck         33*/
!        check that jsu is a valid surface name.                                !/*ck         34*/
   40 if(jsu.ge.1.and.jsu.le.mxj)go to 50                                       !/*ck         35*/
#ifdef MULTT                                                                    /*!ckxk        7*/
      call thread_lockon(jlock)                                                 !/*ckxk        8*/
#endif                                                                          /*!ckxk        9*/
      write(iuo,'(/6h jsu =,i9)')jsu                                            !/*ck         37*/
      call expirx(1,'chksrc',                                                   !/*ck         38*/
     & 'jsu set by source is not a valid program name of a surface.')           !/*ck         39*/
      return                                                                    !/*ck         40*/
!                                                                               !/*ck         41*/
!        check that jsu is a bounding surface of cell icl.                      !/*ck         42*/
   50 do 60 j=abs(lca(llca+icl)),abs(lca(llca+icl+1))-1                         !/*ck         43*/
   60 if(jsu.eq.abs(lja(llja+j)))go to 80                                       !/*ck         44*/
#ifndef HPUX                                                                    /*!ckx         9*/
#ifdef MULTT                                                                    /*!ckxk       10*/
      call thread_lockon(jlock)                                                 !/*ckxk       11*/
#endif                                                                          /*!ckxk       12*/
#endif                                                                          /*!ckx        11*/
      write(iuo,70)icl,ncl(lncl+icl),jsu,nsf(lnsf+jsu)                          !/*ck         46*/
   70 format(/22h cell:  program name =,i4,4x,14hproblem name =,i5/             !/*ck         47*/
     & 25h surface:  program name =,i4,4x,14hproblem name =,i7)                 !/*ck         48*/
      call expirx(1,'chksrc',                                                   !/*ck         49*/
     & 'the source surface is not a surface of the source cell.')               !/*ck         50*/
      return                                                                    !/*ck         51*/
!                                                                               !/*ck         52*/
!        check that uuu,vvv,www is directed into cell icl.                      !/*ck         53*/
   80 call chkcel(icl,2,j)                                                      !/*ck         54*/
      if(j.eq.0)go to 100                                                       !/*ck         55*/
      if(iq.ne.0)call chkcel(iq,2,j)                                            !/*ck         56*/
#ifndef HPUX                                                                    /*!ckx        12*/
#ifdef MULTT                                                                    /*!ckxk       13*/
      call thread_lockon(jlock)                                                 !/*ckxk       14*/
#endif                                                                          /*!ckxk       15*/
#endif                                                                          /*!ckx        14*/
      if(iq.ne.0.and.j.eq.0)write(iuo,10)                                       !/*ck         58*/
      write(iuo,90)uuu,vvv,www,xxx,yyy,zzz,icl,ncl(lncl+icl),jsu,               !/*ck         59*/
     & nsf(lnsf+jsu)                                                            !/*ck         60*/
   90 format(/6h uuu =,1pe12.5,4x,5hvvv =,e12.5,4x,5hwww =,e12.5/               !/*ck         61*/
     & 6h xxx =,1pe12.5,4x,5hyyy =,e12.5,4x,5hzzz =,e12.5/                      !/*ck         62*/
     & 13h cell:  icl =,i4,4x,14hproblem name =,i5/                             !/*ck         63*/
     & 16h surface:  jsu =,i4,4x,14hproblem name =,i7)                          !/*ck         64*/
      call expirx(1,'chksrc',                                                   !/*ck         65*/
     & 'jsu or icl of the source is inconsistent with xxx,yyy,zzz.')            !/*ck         66*/
      return                                                                    !/*ck         67*/
!                                                                               !/*ck         68*/
!        check that uuu,vvv,www is a unit vector.                               !/*ck         69*/
  100 if(abs(uuu**2+vvv**2+www**2-1.).gt.1.e-6)call expirx(1,'chksrc',          !/*ck         70*/
     & 'the uuu,vvv,www vector from source is not the right length.')           !/*ck         71*/
      return                                                                    !/*ck         72*/
      end                                                                       !/*ck         73*/
