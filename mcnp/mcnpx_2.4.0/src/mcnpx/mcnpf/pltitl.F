!#ident pltitl.F from deck pw                                                   !/*pw          1*/
      subroutine pltitl                                                         !/*pw          3*/
#ifdef MCPLOT                                                                   /*!pwx         1*/
!        create the main title for a plot.                                      !/*pw          4*/
      use global_data                                                           !/*pwx         2*/
      use mcplot_data                                                           !/*pwx         3*/
!                                                                               !/*pwx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*pwx         5*/
      implicit integer(kindi) (i-n)                                             !/*pwx         6*/
!                                                                               !/*pwx         7*/
      character ha*80,hf*8                                                      !/*pw          7*/
!                                                                               !/*pw          8*/
!        create as specific a title as possible.                                !/*pw          9*/
      if(ititle(5)+ititle(6).ne.0)go to 90                                      !/*pw         10*/
      titles(5)=' '                                                             !/*pw         11*/
      if(itfc.gt.30)go to 30                                                    !/*pw4a        1*/
      if(kxsplt.ne.0)go to 32                                                   !/*pw4b        1*/
      ha=' '                                                                    !/*pw         13*/
      i=jptal(ljpt+6,mtal)                                                      !/*pw         14*/
      if(i.eq.0)go to 20                                                        !/*pw         15*/
      do 10 j=i+1,i+25                                                          !/*pw         16*/
   10 ha((j-i)*3+1:(j-i)*3+3)=char(itds(j)/65536)//                             !/*pw         17*/
     & char(mod(itds(j),65536)/256)//char(mod(itds(j),256))                     !/*pw         18*/
      if(ha.ne.' ')go to 40                                                     !/*pw         19*/
   20 hf=runtpe                                                                 !/*pw         20*/
      if(ktfile.eq.2)hf=mctal                                                   !/*pw         21*/
      write(ha,'(5hfile ,a8,11h --- tally ,i4)')hf,jptal(ljpt+1,mtal)           !/*pw         22*/
      if(itfc.ne.0)ha(41:80)='tally fluctuation data'                           !/*pw         23*/
      if(itfc.gt.5)ha(41:80)='tally fluctuation chart bin tallies'              !/*pw4a        2*/
      go to 40                                                                  !/*pw         24*/
   30 ha='kcode data from file '//runtpe                                        !/*pw         25*/
      go to 40                                                                  !/*pw4b        2*/
!                                                                               !/*pw4b        3*/
!        cross section plots.                                                   !/*pw4b        4*/
   32 write(ha,'(18hcross section plot)')                                       !/*pw4b        5*/
      do 34 j=jlbl(1,kxsptp),jlbl(2,kxsptp)                                     !/*pw4b        6*/
   34 if(kxspmt.eq.klbl(j))go to 36                                             !/*pw4b        7*/
      j=jlbl(2,kxsptp)                                                          !/*pw4b        8*/
   36 ha(41:80)=hlbl(j)                                                         !/*pw4b        9*/
!                                                                               !/*pw         26*/
!        divide the title into two lines if necessary.                          !/*pw         27*/
   40 call ljusti(ha)                                                           !/*pw         28*/
      titles(6)=ha                                                              !/*pw         29*/
      if(leng(ha).le.40)go to 90                                                !/*pw         30*/
      do 50 i=0,39                                                              !/*pw         31*/
   50 if(ha(40-i:40-i).eq.' ')go to 60                                          !/*pw         32*/
   60 if(i.eq.40.or.leng(ha)+i.gt.80)i=0                                        !/*pw         33*/
      do 70 i=41-i,79                                                           !/*pw         34*/
   70 if(ha(i:i).ne.' ')go to 80                                                !/*pw         35*/
   80 titles(5)=ha(1:i-1)                                                       !/*pw         36*/
      titles(6)=ha(i:80)                                                        !/*pw         37*/
!                                                                               !/*pw         38*/
!        center each line in 40 spaces.                                         !/*pw         39*/
   90 do 100 i=5,6                                                              !/*pw         40*/
      ha=titles(i)                                                              !/*pw         41*/
      if(ha.eq.' ')go to 100                                                    !/*pw         42*/
      call ljusti(ha)                                                           !/*pw         43*/
      l=leng(ha)+1                                                              !/*pw         44*/
      titles(i)=' '                                                             !/*pw         45*/
      titles(i)(21-l/2:40)=ha                                                   !/*pw         46*/
  100 continue                                                                  !/*pw         47*/
#endif                                                                          /*!pwx         8*/
      return                                                                    !/*pw         48*/
      end                                                                       !/*pw         49*/
