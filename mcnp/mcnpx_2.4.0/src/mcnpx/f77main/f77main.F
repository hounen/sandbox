!     $Id: f77main.F,v 1.7 2002/03/27 18:14:38 gwm Exp $                      !/*f7          1*/
!       *****************************************************                   !/*f7          2*/
!       *                                                   *                   !/*f7          3*/
!       *            Copyright Notice for MCNPX             *                   !/*f7          4*/
!       *                                                   *                   !/*f7          5*/
!       * This program was prepared by the Regents of the   *                   !/*f7          6*/
!       * University of California at Los Alamos National   *                   !/*f7          7*/
!       * Laboratory (the University) under contract number *                   !/*f7          8*/
!       * W-7405-ENG-36 with the U.S. Department of Energy  *                   !/*f7          9*/
!       * (DOE).  The University has certain rights in the  *                   !/*f7         10*/
!       * program pursuant to the contract and the program  *                   !/*f7         11*/
!       * should not be copied or distributed outside your  *                   !/*f7         12*/
!       * organization.   All rights in the program are     *                   !/*f7         13*/
!       * reserved by the DOE and the University.  Neither  *                   !/*f7         14*/
!       * the U.S. Government nor the University makes any  *                   !/*f7         15*/
!       * warranty, express or implied, or assumes any      *                   !/*f7         16*/
!       * liability or responsibility for the use of this   *                   !/*f7         17*/
!       * software.                                         *                   !/*f7         18*/
!       *                                                   *                   !/*f7         19*/
!       *****************************************************                   !/*f7         20*/
#if defined (SHARED)                                                            /*!f7         21*/
      subroutine mcnpx_init_module                                              !/*f7         22*/
      call mcnp                                                                 !/*f7         23*/
      return                                                                    !/*f7         24*/
      end                                                                       !/*f7         25*/
#else                                                                           /*!f7         26*/
      program main                                                              !/*f7         27*/
      call mcnp                                                                 !/*f7         28*/
      end                                                                       !/*f7         29*/
#endif                                                                          /*!f7         30*/
