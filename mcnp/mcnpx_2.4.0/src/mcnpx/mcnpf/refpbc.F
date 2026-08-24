!#ident refpbc.F from deck rb4a                                                 !/*rb4a        1*/
      subroutine refpbc                                                         !/*rb4a        2*/
!        reflect off periodic boundary condition surface.                       !/*rb4a        3*/
      use global_data                                                           !/*rbx         1*/
!                                                                               !/*rbx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*rbx         3*/
      implicit integer(kindi) (i-n)                                             !/*rbx         4*/
!                                                                               !/*rbx         5*/
!                                                                               !/*rb4a        5*/
      if(krflg.ne.0)call eventp(3)                                              !/*rb4a        6*/
      m=ksu(lksu+jsu)                                                           !/*rb4a        7*/
      jsu=nint(trf(ltrf+1,m))                                                   !/*rb4a        8*/
      call contrib_pwb (ipt,ipwb_external_exiting,icl,-wgt)                     !/*rbx         6*/
      icl=nint(trf(ltrf+17,m))                                                  !/*rb4a       10*/
      call contrib_pwb (ipt,ipwb_external_exiting,icl,wgt)                      !/*rbx         7*/
      if(lev.ne.0)go to 20                                                      !/*rb4a       12*/
      do 10 i=1,6                                                               !/*rb4a       13*/
   10 udt(i,0)=gpblcm(i)                                                        !/*rb4a       14*/
   20 do 30 i=1,3                                                               !/*rb4a       15*/
      gpblcm(i)=udt(1,0)*trf(ltrf+4+i,m)+udt(2,0)*trf(ltrf+7+i,m)+              !/*rb4a       16*/
     & udt(3,0)*trf(ltrf+10+i,m)+trf(ltrf+1+i,m)                                !/*rb4a       17*/
   30 gpblcm(i+3)=udt(4,0)*trf(ltrf+4+i,m)+udt(5,0)*trf(ltrf+7+i,m)+            !/*rb4a       18*/
     & udt(6,0)*trf(ltrf+10+i,m)                                                !/*rb4a       19*/
      lev=0                                                                     !/*rb4a       20*/
      return                                                                    !/*rb4a       21*/
      end                                                                       !/*rb4a       22*/
