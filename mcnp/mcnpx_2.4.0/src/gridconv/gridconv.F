!     $Id: gridconv.F,v 1.5 2002/03/27 18:13:51 gwm Exp $                     !/*gr          1*/
      program gridconv                                                          !/*gr          2*/
!                                                                               !/*gr          3*/
!       This program is designed to convert mesh tally data                     !/*gr          4*/
!       output from MCNPX to input files for use with one                       !/*gr          5*/
!       of several graphics packages.                                           !/*gr          6*/
!                                                                               !/*gr          7*/
!       This program first decides if this data file is for                     !/*gr          8*/
!       a rectangular mesh, an r, z, theta mesh, or does it                     !/*gr          9*/
!       come from a MCTAL file.                                                 !/*gr         10*/
!                                                                               !/*gr         11*/
!       Written by E. C. Snow  11/20/97   (revised: 12/15/2000)                 !/*gr         12*/
!                                                                               !/*gr         13*/
!       The current package selections are:                                     !/*gr         14*/
!          ng=1   IDL <default>                                                 !/*gr         15*/
!          ng=2   PAW (provided that NO_PAW is not set)                         !/*gr         16*/
!          ng=3   Tecplot                                                       !/*gr         17*/
!          ng=4   Gnuplot                                                       !/*gr         18*/
!                                                                               !/*gr         19*/
#include <gridconv/gcdouble.h>                                                  /*!gr         20*/
#include <gridconv/gccom5.h>                                                    /*!gr         21*/
!                                                                               !/*gr         22*/
      external gblock                                                           !/*gr         23*/
      character hf*30                                                           !/*gr         24*/
      logical lv                                                                !/*gr         25*/
!                                                                               !/*gr         26*/
!       Is this data file a mesh tally data file or a MCTAL file?               !/*gr         27*/
   10 write(6,'("Enter the type of data file: mdata or mctal. ")')              !/*gr         28*/
      write(6,'(" <default=mdata>: ")')                                         !/*gr         29*/
      write(6,'("#")')                                                          !/*gr         30*/
      read(5,'(a30)')hfd                                                        !/*gr         31*/
      if(hfd.eq.' ')hfd='mdata'                                                 !/*gr         32*/
      if(hfd(1:5).eq.'mdata') then                                              !/*gr         33*/
!                                                                               !/*gr         34*/
!       Get the name of the mdata input file.                                   !/*gr         35*/
         nt=1                                                                   !/*gr         36*/
         write(6,'("Enter name of input file. <default=mdata>: ")')             !/*gr         37*/
         write(6,'("#")')                                                       !/*gr         38*/
         read(5,'(a30)')hfd                                                     !/*gr         39*/
         if(hfd.eq.' ')hfd='mdata'                                              !/*gr         40*/
         go to 20                                                               !/*gr         41*/
      elseif(hfd(1:5).eq.'mctal') then                                          !/*gr         42*/
!                                                                               !/*gr         43*/
!       Get the name of the mctal input file.                                   !/*gr         44*/
         nt=2                                                                   !/*gr         45*/
         write(6,'("Enter name of input file. <default=mctal>: ")')             !/*gr         46*/
         write(6,'(" <a30 format> ")')                                          !/*gr         47*/
         write(6,'("#")')                                                       !/*gr         48*/
         read(5,'(a30)')hfd                                                     !/*gr         49*/
         if(hfd.eq.' ')hfd='mctal'                                              !/*gr         50*/
         go to 20                                                               !/*gr         51*/
      else                                                                      !/*gr         52*/
         write(6,'(" mdata or mctal are the only entries allowed. ")')          !/*gr         53*/
         go to 10                                                               !/*gr         54*/
      endif                                                                     !/*gr         55*/
!                                                                               !/*gr         56*/
   20 inquire(file=hfd,exist=lv)                                                !/*gr         57*/
      if(.not.lv) then                                                          !/*gr         58*/
         write(6,'(" File ",a30," does not exist!")')hfd                        !/*gr         59*/
         go to 500                                                              !/*gr         60*/
      endif                                                                     !/*gr         61*/
!                                                                               !/*gr         62*/
!       If this is a mdata file call the subroutine rdgd.                       !/*gr         63*/
!       If this is a mctal file call the subroutine rdmctal.                    !/*gr         64*/
!                                                                               !/*gr         65*/
      if(nt.eq.1) then                                                          !/*gr         66*/
         call rdgd                                                              !/*gr         67*/
         go to 500                                                              !/*gr         68*/
      else                                                                      !/*gr         69*/
         call rdmctal                                                           !/*gr         70*/
         go to 500                                                              !/*gr         71*/
      endif                                                                     !/*gr         72*/
!                                                                               !/*gr         73*/
  500 write(6,'("Do you wish to work with another data file. y/n ?")')          !/*gr         74*/
      write(6,'("Type yes(y) or no(n) or hit return to exit.")')                !/*gr         75*/
      write(6,'("#")')                                                          !/*gr         76*/
      read(5,'(a3)')hf                                                          !/*gr         77*/
      if(hf(1:1).eq.'y'.or.hf.eq.'yes')go to 10                                 !/*gr         78*/
      stop                                                                      !/*gr         79*/
!                                                                               !/*gr         80*/
      end                                                                       !/*gr         81*/
