!#ident sourck.F from deck sy                                                   !/*sy          1*/
      subroutine sourck                                                         !/*sy          2*/
!        start a neutron from the kcode source.                                 !/*sy          3*/
!        multitasking lock is left on by trnspt for sourck.                     !/*sy          4*/
      use global_data                                                           !/*syx         1*/
!                                                                               !/*syx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*syx         3*/
      implicit integer(kindi) (i-n)                                             !/*syx         4*/
!                                                                               !/*syx         5*/
      ist0=ist                                                                  !/*sy          6*/
      ixak0=ixak                                                                !/*sy          7*/
      nsa0=nsa                                                                  !/*sy          8*/
      nss0=nss                                                                  !/*sy          9*/
      ipsc=101                                                                  !/*sy         10*/
      call isos(uuu,0)                                                          !/*sy4b        1*/
      l=lfso+(ixak-1)*6                                                         !/*sy         12*/
      xxx=fso(l+1)                                                              !/*sy         13*/
      yyy=fso(l+2)                                                              !/*sy         14*/
      zzz=fso(l+3)                                                              !/*sy         15*/
      erg=fso(l+4)                                                              !/*sy         16*/
      if(mcal.eq.0)go to 10                                                     !/*sy         17*/
      jgp=erg                                                                   !/*sy         18*/
      erg=xss(jxs(ljxs+1,mgegbt(1))+jgp-1)                                      !/*sy         19*/
   10 icl=fso(l+5)                                                              !/*sy         20*/
      wgt=wt0                                                                   !/*sy         21*/
      tme=0.                                                                    !/*sy         22*/
      jsu=0                                                                     !/*sy         23*/
      if(junf.ne.0)call findlv                                                  !/*cor4-2    192*/
      if(ixak.eq.msrk)ixak=0                                                    !/*sy         24*/
      ixak=ixak+1                                                               !/*sy         25*/
      nsa=nsa-1                                                                 !/*sy         26*/
!                                                                               !/*sy         27*/
!        write fission source to surface source if required.                    !/*cor4-2    193*/
      if(nilw.eq.0)return                                                       !/*cor4-2    194*/
      if(kc8.gt.0)return                                                        !/*sy4b        2*/
      do 20 i=njss+1,njss+nilw                                                  !/*cor4-2    196*/
   20 if(jss(ljss+i).eq.ncl(lncl+icl))go to 30                                  !/*cor4-2    197*/
      return                                                                    !/*cor4-2    198*/
   30 call sufwrt(i,zero)                                                       !/*cor4-2    199*/
      return                                                                    !/*sy         74*/
      end                                                                       !/*sy         75*/
