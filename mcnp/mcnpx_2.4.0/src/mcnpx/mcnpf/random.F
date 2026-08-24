!#ident random.F from deck ra                                                   !/*ra          1*/
      subroutine random                                                         !/*ra          2*/
!        controls for the pseudo-random number sequence.                        !/*ra          3*/
      use global_data                                                           !/*rax         1*/
!                                                                               !/*rax         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*rax         3*/
      implicit integer(kindi) (i-n)                                             !/*rax         4*/
!                                                                               !/*rax         5*/
      parameter (fb=13008944d0,fs=170125d0,gb=1136868d0,gs=6328637d0,           !/*ra          7*/
     & p=2d0**24,q=2d0**(-24),rm=5d0**19)                                       !/*ra          8*/
!                                                                               !/*ra          9*/
      nstrid=152917                                                             !/*ra         10*/
      rnfb=fb                                                                   !/*ra         11*/
      rnfs=fs                                                                   !/*ra         12*/
      rngb=gb                                                                   !/*ra         13*/
      rngs=gs                                                                   !/*ra         14*/
      rnmult=rm                                                                 !/*ra         15*/
      if(dbcn(1)+dbcn(8)+dbcn(13)+dbcn(14).ne.0.)write(iuo,'(1h1)')             !/*ra         16*/
!                                                                               !/*ra         17*/
!        set new random number multiplier, rnmult, if required.                 !/*ra         18*/
!        rngb and rngs are the upper and lower 24 bits of rnmult.               !/*ra         19*/
      if(dbcn(13)+dbcn(14).le.0.)go to 30                                       !/*ra         20*/
      if(dbcn(14).gt.0.)rnmult=dbcn(14)                                         !/*ra         21*/
      if(aint((rnmult+.5)*.5).eq.aint((rnmult+1.5)*.5))                         !/*ra         22*/
     & call erprnt(1,1,0,0,0,0,0,0,                                             !/*ra         23*/
     & '44hrandom number multiplier, dbcn(14), is even.')                       !/*ra         24*/
      rngb=aint(rnmult*q)                                                       !/*ra         25*/
      rngs=rnmult-rngb*p                                                        !/*ra         26*/
      if(rngb+rngs.ge.p)call erprnt(1,1,0,0,0,0,0,0,                            !/*ra         27*/
     & '45hrandom number multiplier, dbcn(14), rejected.')                      !/*ra         28*/
!                                                                               !/*ra         29*/
!        get rnfb (upper 24 bits) and rnfs (lower 24 bits) of                   !/*ra         30*/
!        rnmult**nstrid which is used in advijk to advance the random           !/*ra         31*/
!        number by nstrid random numbers for each history.                      !/*ra         32*/
      if(dbcn(13).gt.0.)nstrid=dbcn(13)                                         !/*ra         33*/
      rnfb=rngb                                                                 !/*ra         34*/
      rnfs=rngs                                                                 !/*ra         35*/
      ii=nstrid-1                                                               !/*ra         36*/
      do 10 i=1,ii                                                              !/*ra         37*/
      a=rngs*rnfs                                                               !/*ra         38*/
      b=(rngb*rnfs-aint(rngb*rnfs*q)*p)                                         !/*ra         39*/
     & +(rngs*rnfb-aint(rngs*rnfb*q)*p)+aint(a*q)                               !/*ra         40*/
      rnfs=a-aint(a*q)*p                                                        !/*ra         41*/
   10 rnfb=b-aint(b*q)*p                                                        !/*ra         42*/
      write(iuo,20)nstrid,rnmult                                                !/*ra         43*/
   20 format(/23h random number stride =,i19/                                   !/*ra         44*/
     & 27h random number multiplier =,f16.0,tl1,1h )                            !/*ra         45*/
!                                                                               !/*ra         46*/
!        set the first random number, rijk, composed of                         !/*ra         47*/
!        ranj (top 24 bits) and rani (bottom 24 bits).                          !/*ra         48*/
   30 rijk=rnmult                                                               !/*ra         49*/
      if(dbcn(1).gt.0.)rijk=dbcn(1)                                             !/*ra         50*/
      if(dbcn(1)+dbcn(8).ne.0.)inif=1                                           !/*ra4a        1*/
      rani=aint(rijk*q)                                                         !/*ra         52*/
      ranj=rijk-rani*p                                                          !/*ra         53*/
      k=nint(dbcn(8))                                                           !/*rax         6*/
      if(dbcn(1).ne.0.)k=k-1                                                    !/*rax         7*/
      do 40 i=1,k                                                               !/*rax         8*/
   40 call advijk                                                               !/*ra         55*/
      if(dbcn(1)+dbcn(8).ne.0.)write(iuo,50)rijk                                !/*ra         56*/
   50 format(25h starting random number =,2x,f16.0,tl1,1h )                     !/*ra         57*/
      dbcn(1)=0.                                                                !/*ra         58*/
      dbcn(8)=0.                                                                !/*ra         59*/
      return                                                                    !/*ra         60*/
      end                                                                       !/*ra         61*/
