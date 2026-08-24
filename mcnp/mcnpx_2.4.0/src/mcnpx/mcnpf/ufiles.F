!#ident ufiles.F from deck uf                                                   !/*uf          1*/
      subroutine ufiles                                                         !/*uf          2*/
!        open the extra files specified by the user.                            !/*uf          3*/
      use global_data                                                           !/*ufx         1*/
      use imcn_data                                                             !/*ufx         2*/
!                                                                               !/*ufx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*ufx         4*/
      implicit integer(kindi) (i-n)                                             !/*ufx         5*/
!                                                                               !/*ufx         6*/
!                                                                               !/*uf          7*/
!        do up to 6 user-specified files.                                       !/*uf          8*/
      do 60 jf=1,6                                                              !/*uf          9*/
      if(kufil(1,jf).eq.0)go to 60                                              !/*uf         10*/
!                                                                               !/*uf         11*/
!        replace dummy file name.                                               !/*uf         12*/
      if(ufil(1,jf).eq.'dumn1')ufil(1,jf)=dumn1                                 !/*uf         13*/
      if(ufil(1,jf).eq.'dumn2')ufil(1,jf)=dumn2                                 !/*uf         14*/
!                                                                               !/*uf         15*/
!        check for existing file in continue-run case.                          !/*uf         16*/
      if(konrun.eq.0)go to 30                                                   !/*uf4a        1*/
      if(inqire(ufil(1,jf)).ne.0)call erprnt(1,2,0,0,0,0,0,0,                   !/*uf4a        2*/
     & '45hexisting file '//ufil(1,jf)//' may be overwritten.')                 !/*uf4a        3*/
!                                                                               !/*uf         34*/
!        set up the access method, form, and record length.                     !/*uf         35*/
   30 if(ufil(2,jf).eq.' ')ufil(2,jf)='sequential'                              !/*uf         36*/
      if(ufil(3,jf).ne.' ')go to 40                                             !/*uf         37*/
      ufil(3,jf)='formatted'                                                    !/*uf         38*/
      if(ufil(2,jf).eq.'direct')ufil(3,jf)='unformatted'                        !/*uf         39*/
   40 if(ufil(2,jf).ne.'direct'.or.kufil(2,jf).gt.0)go to 50                    !/*uf         40*/
      call erprnt(1,1,0,0,0,0,0,0,'49hrecord length for '//                     !/*uf         41*/
     & ufil(1,jf)(1:8)//' on files card is zero.')                              !/*uf         42*/
      go to 60                                                                  !/*uf         43*/
!                                                                               !/*uf         44*/
!        try to open (create) the file.                                         !/*uf         45*/
   50 if(ufil(2,jf).ne.'direct')open(kufil(1,jf),file=ufil(1,jf),               !/*uf         46*/
     & form=ufil(3,jf),iostat=io,status='unknown')                              !/*uf         47*/
      if(ufil(2,jf).eq.'direct')open(kufil(1,jf),file=ufil(1,jf),               !/*uf         48*/
     & form=ufil(3,jf),access='direct',recl=kufil(2,jf),iostat=io,              !/*uf         49*/
     & status='unknown')                                                        !/*uf         50*/
      if(io.ne.0)call erprnt(1,1,1,io,0,0,0,0,'47hunable to open '//            !/*uf         51*/
     & ufil(1,jf)(1:8)//' on files card.  error =,i6')                          !/*uf         52*/
!                                                                               !/*uf         53*/
   60 continue                                                                  !/*uf         54*/
      return                                                                    !/*uf         55*/
      end                                                                       !/*uf         56*/
