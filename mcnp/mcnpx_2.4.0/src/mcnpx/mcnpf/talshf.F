!#ident talshf.F from deck ts                                                   !/*ts          1*/
      subroutine talshf                                                         !/*ts          2*/
!        add the tallies for this history to the totals for all                 !/*ts          3*/
!        histories and sum history tally powers for error estimates.            !/*ts4a        1*/
      use global_data                                                           !/*tsx         1*/
!                                                                               !/*tsx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*tsx         3*/
      implicit integer(kindi) (i-n)                                             !/*tsx         4*/
!                                                                               !/*tsx         5*/
!                                                                               !/*ts          6*/
!        save quantities for a history tally in the tfc bin.                    !/*ts4a        2*/
      do 10 ip=0,npert                                                          !/*ts4b        1*/
      do 10 ital=1,ntal                                                         !/*ts4b        2*/
      j=ktal+jptal(ljpt+7,ital)+ip*mxfp                                         !/*ts4b        3*/
      if(tal(j).eq.0.)go to 10                                                  !/*ts4b        4*/
      t=tal(j)                                                                  !/*ts4b        5*/
      it=ital+ip*ntal                                                           !/*ts4b        6*/
!                                                                               !/*ts4a        5*/
!        save the tfc bin third and fourth moments for vov calculation.         !/*ts4a        6*/
      shsd(kshs+nsp+3,it)=shsd(kshs+nsp+3,it)+t**3                              !/*ts4a        7*/
      shsd(kshs+nsp+4,it)=shsd(kshs+nsp+4,it)+t**4                              !/*ts4a        8*/
!                                                                               !/*ts4a        9*/
!        switch sign of t if f(-x) is requested by negative dbcn(16).           !/*ts4a       10*/
      t=hsb(nsp)*t                                                              !/*ts4a       11*/
      if(t.gt.ddm(kddm+1,it))ddm(kddm+2,it)=npstc                               !/*ts4a       12*/
      ddm(kddm+1,it)=max(ddm(kddm+1,it),t)                                      !/*ts4a       13*/
!                                                                               !/*ts4a       14*/
!        increment tfc history tally distribution arrays nhsd and shsd.         !/*ts4a       15*/
!        the -4 location is for negative scores and -3 is total scores.         !/*ts4a       16*/
      j=max(1,min(nsp,int(10.*log10(abs(t))+nhb)))                              !/*ts4a       17*/
      if(t.gt.0.)go to 2                                                        !/*ts4a       18*/
      j=1                                                                       !/*ts4a       19*/
      nhsd(knhs+nsp+1,it)=nhsd(knhs+nsp+1,it)+1                                 !/*ts4a       20*/
      shsd(kshs+nsp+1,it)=shsd(kshs+nsp+1,it)+t                                 !/*ts4a       21*/
    2 nhsd(knhs+j,it)=nhsd(knhs+j,it)+1                                         !/*ts4a       22*/
      nhsd(knhs+nsp+2,it)=nhsd(knhs+nsp+2,it)+1                                 !/*ts4a       23*/
      shsd(kshs+j,it)=shsd(kshs+j,it)+t                                         !/*ts4a       24*/
      shsd(kshs+nsp+2,it)=shsd(kshs+nsp+2,it)+t                                 !/*ts4a       25*/
!                                                                               !/*ts4a       26*/
!        save the ntp largest,smallest history tallies for each tfc bin.        !/*ts4a       27*/
      if(t.le.shsd(kshs+nsp+ntp+6,it))go to 10                                  !/*ts4a       28*/
      if(nhsd(knhs+nsp+6,it).eq.ntp)go to 4                                     !/*ts4a       29*/
      nhsd(knhs+nsp+6,it)=nhsd(knhs+nsp+6,it)+1                                 !/*ts4a       30*/
      shsd(kshs+nsp+5+nhsd(knhs+nsp+6,it),it)=t                                 !/*ts4a       31*/
      go to 10                                                                  !/*ts4a       32*/
    4 nhsd(knhs+nsp+7,it)=nhsd(knhs+nsp+7,it)+1                                 !/*ts4a       33*/
      stt(kstt+nhsd(knhs+nsp+7,it),it)=t                                        !/*ts4a       34*/
      if(nhsd(knhs+nsp+7,it).eq.ntp)call hpsort(it,knhs+nsp+5,                  !/*ts4a       35*/
     & kshs+nsp+5,ntp,ntp)                                                      !/*ts4a       36*/
   10 continue                                                                  !/*ts4a       37*/
      if(jtls.le.ktls)go to 30                                                  !/*ts         11*/
!                                                                               !/*ts         12*/
!        table-scoring case: create two do loops to allow vectorization.        !/*ts4a       38*/
      if(nmxf.eq.5)go to 23                                                     !/*ts4a       39*/
!dir$ ivdep                                                                     !/*ts         14*/
      do 20 i=1,mxf                                                             !/*ts         15*/
#ifndef UNICOS                                                                  /*!ts4b        7*/
!        omit the check, in order for the loop to vectorize.                    !/*tsx         6*/
      if(tal(ktal+i).eq.0.)go to 20                                             !/*ts         17*/
#endif                                                                          /*!cpp       272*/
      tal(ktal+mxf+i)=tal(ktal+mxf+i)+tal(ktal+i)                               !/*ts         18*/
      tal(ktal+2*mxf+i)=tal(ktal+2*mxf+i)+tal(ktal+i)**2                        !/*ts         19*/
      tal(ktal+i)=0.                                                            !/*ts         20*/
   20 continue                                                                  !/*ts         21*/
      return                                                                    !/*ts         22*/
!                                                                               !/*ts4a       40*/
!        sum first four moments of history tallies for errors and vov.          !/*ts4a       41*/
!dir$ ivdep                                                                     !/*ts4a       42*/
   23 do 25 i=1,mxf                                                             !/*ts4a       43*/
#ifndef UNICOS                                                                  /*!ts4b        8*/
!        omit the check, in order for the loop to vectorize.                    !/*tsx         7*/
      if(tal(ktal+i).eq.0.)go to 25                                             !/*ts4a       45*/
#endif                                                                          /*!cpp       273*/
      tal(ktal+mxf+i)=tal(ktal+mxf+i)+tal(ktal+i)                               !/*ts4a       46*/
      tal(ktal+2*mxf+i)=tal(ktal+2*mxf+i)+tal(ktal+i)**2                        !/*ts4a       47*/
      tal(ktal+3*mxf+i)=tal(ktal+3*mxf+i)+tal(ktal+i)**3                        !/*ts4a       48*/
      tal(ktal+4*mxf+i)=tal(ktal+4*mxf+i)+tal(ktal+i)**4                        !/*ts4a       49*/
      tal(ktal+i)=0.                                                            !/*ts4a       50*/
   25 continue                                                                  !/*ts4a       51*/
      return                                                                    !/*ts4a       52*/
!                                                                               !/*ts         23*/
!        list-scoring case: create two do loops to allow vectorization.         !/*ts4a       53*/
   30 if(nmxf.eq.5)go to 50                                                     !/*ts4a       54*/
      do 40 ip=0,npert                                                          !/*ts4b        9*/
      do 40 i=1,jtls                                                            !/*ts4a       55*/
      j=tal(ktal+3*mxf+i)+ip*mxfp                                               !/*ts4b       10*/
      tal(j+mxf)=tal(j+mxf)+tal(j)                                              !/*ts         27*/
      tal(j+2*mxf)=tal(j+2*mxf)+tal(j)**2                                       !/*ts         28*/
   40 tal(j)=0.                                                                 !/*ts         29*/
      return                                                                    !/*ts         30*/
!                                                                               !/*ts4a       56*/
!        sum first four moments of history tallies for errors and vov.          !/*ts4a       57*/
   50 do 60 ip=0,npert                                                          !/*ts4b       11*/
      do 60 i=1,jtls                                                            !/*ts4b       12*/
      j=tal(ktal+5*mxf+i)+ip*mxfp                                               !/*ts4b       13*/
      tal(j+mxf)=tal(j+mxf)+tal(j)                                              !/*ts4a       60*/
      tal(j+2*mxf)=tal(j+2*mxf)+tal(j)**2                                       !/*ts4a       61*/
      tal(j+3*mxf)=tal(j+3*mxf)+tal(j)**3                                       !/*ts4a       62*/
      tal(j+4*mxf)=tal(j+4*mxf)+tal(j)**4                                       !/*ts4a       63*/
   60 tal(j)=0.                                                                 !/*ts4a       64*/
      return                                                                    !/*ts4a       65*/
      end                                                                       !/*ts         31*/
