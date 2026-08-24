!     $Id: count_spb_data.F,v 1.5 2002/03/27 18:16:19 gwm Exp $               !/*co          1*/
      integer function count_spb_data()                                         !/*co          2*/
!     Count by secondary particle type NUM_ENERGY_BANDS(), the number of        !/*co          3*/
!     energy-band/biasing-parameter pairs.  Also keep a running total of        !/*co          4*/
!     LEN_DYNAMIC_SPB_DATA, the length of dynamically allocated storage         !/*co          5*/
!     for secondary-particle biasing.  Return the number of energy bands        !/*co          6*/
!     requested for the particular SPABI card.                                  !/*co          7*/
!                                                                               !/*co          8*/
#ifdef SPABI                                                                    /*!co          9*/
      use global_data                                                           !/*coa         1*/
      use imcn_data                                                             !/*coa         2*/
!                                                                               !/*coa         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*coa         4*/
      implicit integer(kindi) (i-n)                                             !/*coa         5*/
!                                                                               !/*co         12*/
      count_spb_data = 0                                                        !/*co         13*/
!                                                                               !/*co         14*/
      nn = (nwc-1)/2                                                            !/*co         15*/
      if (nn.lt.1) then                                                         !/*co         16*/
         call erprnt(2,1,0,0,0,0,0,0,                                           !/*co         17*/
     &    '41hSPABI card fails to specify energy bands.')                       !/*co         18*/
         return                                                                 !/*co         19*/
      endif                                                                     !/*co         20*/
!                                                                               !/*co         21*/
      if (mod(nwc-1,2).ne.0) then                                               !/*co         22*/
         call erprnt(2,1,0,0,0,0,0,0,                                           !/*co         23*/
     &    '42hThe SPABI energy entries must be in pairs.')                      !/*co         24*/
         return                                                                 !/*co         25*/
      endif                                                                     !/*co         26*/
!                                                                               !/*co         27*/
      do i=1,mipt                                                               !/*co         28*/
         if (nqp(i).eq.1) then                                                  !/*co         29*/
            num_energy_bands(i)=nn                                              !/*co         30*/
            count_spb_data = nn                                                 !/*co         31*/
            call increment_spb_length(nwc-1)                                    !/*co         32*/
            return                                                              !/*co         33*/
         endif                                                                  !/*co         34*/
      enddo                                                                     !/*co         35*/
!     (The error message should be given elsewhere.)                            !/*co         36*/
!                                                                               !/*co         37*/
#else                                                                           /*!co         38*/
      count_spb_data = 0                                                        !/*co         39*/
#endif                                                                          /*!co         40*/
      return                                                                    !/*co         41*/
      end                                                                       !/*co         42*/
