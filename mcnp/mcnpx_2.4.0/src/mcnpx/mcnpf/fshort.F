!#ident fshort.F from deck ft                                                   !/*ft          1*/
      subroutine fshort(ar,ha,nc)                                               !/*ft          2*/
!        convert ar into the shortest possible character string ha,             !/*ft          3*/
!        nc characters long, retaining at least 5 digits of precision.          !/*ft          4*/
      use global_parameters                                                     !/*ftx         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*ftx         2*/
      implicit integer(kindi) (i-n)                                             !/*ftx         3*/
      character ha*10,ht*15                                                     !/*ft          7*/
!                                                                               !/*ft          8*/
      aa=abs(ar)                                                                !/*ft          9*/
!                                                                               !/*ft         10*/
!        process aa = integer, < 100000000.                                     !/*ft         11*/
      if(aa.ne.aint(aa).or.aa.gt.100000000.)go to 10                            !/*ft         12*/
      nc=1+int(log10(.5+aa))                                                    !/*ft         13*/
      write(ht,'(i9)')int(aa)                                                   !/*ft         14*/
      ha=ht(10-nc:9)                                                            !/*ft         15*/
      go to 140                                                                 !/*ft         16*/
!                                                                               !/*ft         17*/
!        process aa .ge. .1 and < 10000000.                                     !/*ft         18*/
   10 if(aa.ge.10000000.)go to 110                                              !/*ft         19*/
      if(aa.lt..1)go to 60                                                      !/*ft         20*/
      write(ht,'(f15.5)')aa                                                     !/*ft         21*/
      if(ht(2:2).ne.' ')go to 110                                               !/*ft         22*/
      do 20 i=10,15                                                             !/*ft         23*/
      nc=25-i                                                                   !/*ft         24*/
   20 if(ht(nc:nc).ne.'0')go to 30                                              !/*ft         25*/
   30 do 40 n=3,10                                                              !/*ft         26*/
   40 if(ht(n:n).ne.'0'.and.ht(n:n).ne.' ')go to 50                             !/*ft         27*/
   50 nc=max(11-n,min(nc+1-n,6))                                                !/*ft         28*/
      ha=ht(n:n+nc-1)                                                           !/*ft         29*/
      go to 140                                                                 !/*ft         30*/
!                                                                               !/*ft         31*/
!        process aa .ge. .0000001 and < .1                                      !/*ft         32*/
   60 if(aa.lt..0000001)go to 110                                               !/*ft         33*/
      write(ht,'(f13.11)')aa                                                    !/*ft         34*/
      do 70 i=2,13                                                              !/*ft         35*/
      nc=15-i                                                                   !/*ft         36*/
   70 if(ht(nc:nc).ne.'0')go to 80                                              !/*ft         37*/
   80 do 90 n=3,9                                                               !/*ft         38*/
   90 if(ht(n:n).ne.'0')go to 100                                               !/*ft         39*/
  100 if(n.ge.6.and.nc.gt.9)go to 110                                           !/*ft         40*/
      nc=min(8,nc-1)                                                            !/*ft         41*/
      ha=ht(2:nc+1)                                                             !/*ft         42*/
      go to 140                                                                 !/*ft         43*/
!                                                                               !/*ft         44*/
!        process other aa.                                                      !/*ft         45*/
  110 write(ht,'(1pe11.4)')aa                                                   !/*ft         46*/
      do 120 i=3,7                                                              !/*ft         47*/
      n=10-i                                                                    !/*ft         48*/
  120 if(ht(n:n).ne.'0')go to 130                                               !/*ft         49*/
  130 nc=10                                                                     !/*ft         50*/
      if(ht(10:10).eq.'0')nc=11                                                 !/*ft         51*/
      ha=ht(2:n)//ht(9:9)//ht(nc:11)                                            !/*ft         52*/
      nc=n-nc+12                                                                !/*ft         53*/
!                                                                               !/*ft         54*/
!        add minus sign if necessary.                                           !/*ft         55*/
  140 if(ar.ge.0.)return                                                        !/*ft         56*/
      nc=nc+1                                                                   !/*ft         57*/
      ht=ha                                                                     !/*ft         58*/
      ha='-'//ht                                                                !/*ft         59*/
      return                                                                    !/*ft         60*/
      end                                                                       !/*ft         61*/
