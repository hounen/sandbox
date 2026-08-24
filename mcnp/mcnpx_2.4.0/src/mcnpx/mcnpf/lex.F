!#ident lex.F from deck xlx                                                   /*xlx         1*/
      subroutine lex(iu)                                                        !/*xlx         2*/
!        write copyright notice on unit iu.                                     !/*xlx         3*/
      parameter (ln=19)                                                         !/*xlx         4*/
      character lx(ln)*53                                                       !/*xlx         5*/
      data lx /                                                                 !/*xlx         6*/
     & '*****************************************************',                 !/*xlx         7*/
     & '*                                                   *',                 !/*xlx         8*/
     & '*            Copyright Notice for MCNPX             *',                 !/*xlx         9*/
     & '*                                                   *',                 !/*xlx        10*/
     & '* This program was prepared by the Regents of the   *',                 !/*xlx        11*/
     & '* University of California at Los Alamos National   *',                 !/*xlx        12*/
     & '* Laboratory (the University) under contract number *',                 !/*xlx        13*/
     & '* W-7405-ENG-36 with the U.S. Department of Energy  *',                 !/*xlx        14*/
     & '* (DOE).  The University has certain rights in the  *',                 !/*xlx        15*/
     & '* program pursuant to the contract and the program  *',                 !/*xlx        16*/
     & '* should not be copied or distributed outside your  *',                 !/*xlx        17*/
     & '* organization.   All rights in the program are     *',                 !/*xlx        18*/
     & '* reserved by the DOE and the University.  Neither  *',                 !/*xlx        19*/
     & '* the U.S. Government nor the University makes any  *',                 !/*xlx        20*/
     & '* warranty, express or implied, or assumes any      *',                 !/*xlx        21*/
     & '* liability or responsibility for the use of this   *',                 !/*xlx        22*/
     & '* software.                                         *',                 !/*xlx        23*/
     & '*                                                   *',                 !/*xlx        24*/
     & '*****************************************************'/                 !/*xlx        25*/
!                                                                               !/*xlx        26*/
      do 100 i=1,ln                                                             !/*xlx        27*/
  100 write(iu,'(1x,a53)')lx(i)                                                 !/*xlx        28*/
!                                                                               !/*xlx        29*/
      return                                                                    !/*xlx        30*/
      end                                                                       !/*xlx        31*/
