!#ident GLOBAL4_cg.F from deck mam                                            /*mam         1*/
      module allocate_memory                                                    !/*mam         2*/
!                                                                               !/*mam         3*/
      use global_parameters                                                     !/*mam         4*/
!                                                                               !/*mam         5*/
!        Provides a generic reallocation function that can                      !/*mam         6*/
!        act on allocated pointers.                                             !/*mam         7*/
!        Currently supports 1D integer, real and real(8)                        !/*mam         8*/
!        arrays and 2D integer arrays.                                          !/*mam         9*/
!                                                                               !/*mam        10*/
      implicit none                                                             !/*mam        11*/
      interface chg_mem                                                         !/*mam        12*/
        module procedure chg_i                                                  !/*mam        13*/
        module procedure chg_i2                                                 !/*mam        14*/
        module procedure chg_r                                                  !/*mam        15*/
        module procedure chg_d                                                  !/*mam        16*/
      end interface                                                             !/*mam        17*/
      contains                                                                  !/*mam        18*/
      function chg_i (in_array,nl1,ist) result(new_array)                       !/*mam        19*/
!        Description:                                                           !/*mam        20*/
!        Reallocates a 1D integer array, preserving its contents.               !/*mam        21*/
        integer(kindi), intent(in) :: nl1                                       !/*mam        22*/
        integer(kindi), pointer :: in_array(:), new_array(:)                    !/*mam        23*/
        integer(kindi), intent(out) :: ist                                      !/*mam        24*/
        integer(kindi) :: n1                                                    !/*mam        25*/
        allocate (new_array(nl1), stat=ist)                                     !/*mam        26*/
        if (ist /= 0) then                                                      !/*mam        27*/
          nullify (new_array)                                                   !/*mam        28*/
        else                                                                    !/*mam        29*/
          new_array=0                                                           !/*mam        30*/
          if (associated(in_array)) then                                        !/*mam        31*/
            n1=size(in_array)                                                   !/*mam        32*/
            n1=min(n1,nl1)                                                      !/*mam        33*/
            new_array(1:n1)=in_array                                            !/*mam        34*/
            deallocate (in_array,stat=ist)                                      !/*mam        35*/
          endif                                                                 !/*mam        36*/
          if (ist == 0) then                                                    !/*mam        37*/
            in_array => new_array                                               !/*mam        38*/
          else                                                                  !/*mam        39*/
            deallocate (new_array)                                              !/*mam        40*/
            nullify (new_array)                                                 !/*mam        41*/
          endif                                                                 !/*mam        42*/
        endif                                                                   !/*mam        43*/
        return                                                                  !/*mam        44*/
        end function chg_i                                                      !/*mam        45*/
      function chg_i2 (in_array,nl1,nl2,ist) result(new_array)                  !/*mam        46*/
!        Description:                                                           !/*mam        47*/
!        Reallocates a 2D integer array, preserving its contents.               !/*mam        48*/
        integer(kindi), intent(in) :: nl1, nl2                                  !/*mam        49*/
        integer(kindi), pointer :: in_array(:,:), new_array(:,:)                !/*mam        50*/
        integer(kindi), intent(out) :: ist                                      !/*mam        51*/
        integer(kindi) :: n1, n2                                                !/*mam        52*/
        allocate (new_array(nl1,nl2), stat=ist)                                 !/*mam        53*/
        if (ist /= 0) then                                                      !/*mam        54*/
          nullify (new_array)                                                   !/*mam        55*/
        else                                                                    !/*mam        56*/
          new_array=0                                                           !/*mam        57*/
          if (associated(in_array)) then                                        !/*mam        58*/
            n1=size(in_array,1)                                                 !/*mam        59*/
            n1=min(n1,nl1)                                                      !/*mam        60*/
            n2=size(in_array,2)                                                 !/*mam        61*/
            n2=min(n2,nl2)                                                      !/*mam        62*/
            new_array(1:n1,1:n2)=in_array                                       !/*mam        63*/
            deallocate (in_array,stat=ist)                                      !/*mam        64*/
          endif                                                                 !/*mam        65*/
          if (ist == 0) then                                                    !/*mam        66*/
            in_array => new_array                                               !/*mam        67*/
          else                                                                  !/*mam        68*/
            deallocate (new_array)                                              !/*mam        69*/
            nullify (new_array)                                                 !/*mam        70*/
          endif                                                                 !/*mam        71*/
        endif                                                                   !/*mam        72*/
        return                                                                  !/*mam        73*/
        end function chg_i2                                                     !/*mam        74*/
      function chg_r (in_array,nl1,ist) result(new_array)                       !/*mam        75*/
!        Description:                                                           !/*mam        76*/
!        Reallocates a 1D real array, preserving its contents.                  !/*mam        77*/
        integer(kindi), intent(in) :: nl1                                       !/*mam        78*/
        real, pointer :: in_array(:), new_array(:)                              !/*mam        79*/
        integer(kindi), intent(out) :: ist                                      !/*mam        80*/
        integer(kindi) :: n1                                                    !/*mam        81*/
        allocate (new_array(nl1), stat=ist)                                     !/*mam        82*/
        if (ist /= 0) then                                                      !/*mam        83*/
          nullify (new_array)                                                   !/*mam        84*/
        else                                                                    !/*mam        85*/
          new_array=0                                                           !/*mam        86*/
          if (associated(in_array)) then                                        !/*mam        87*/
            n1=size(in_array)                                                   !/*mam        88*/
            n1=min(n1,nl1)                                                      !/*mam        89*/
            new_array(1:n1)=in_array                                            !/*mam        90*/
            deallocate (in_array,stat=ist)                                      !/*mam        91*/
          endif                                                                 !/*mam        92*/
          if (ist == 0) then                                                    !/*mam        93*/
            in_array => new_array                                               !/*mam        94*/
          else                                                                  !/*mam        95*/
            deallocate (new_array)                                              !/*mam        96*/
            nullify (new_array)                                                 !/*mam        97*/
          endif                                                                 !/*mam        98*/
        endif                                                                   !/*mam        99*/
        return                                                                  !/*mam       100*/
        end function chg_r                                                      !/*mam       101*/
      function chg_d (in_array,nl1,ist) result(new_array)                       !/*mam       102*/
!        Description:                                                           !/*mam       103*/
!        Reallocates a 1D real(8) array, preserving its contents.               !/*mam       104*/
        integer(kindi), intent(in) :: nl1                                       !/*mam       105*/
        real(kindr), pointer :: in_array(:), new_array(:)                       !/*mam       106*/
        integer(kindi), intent(out) :: ist                                      !/*mam       107*/
        integer(kindi) :: n1,i                                                  !/*mam       108*/
        allocate (new_array(nl1), stat=ist)                                     !/*mam       109*/
        if (ist /= 0) then                                                      !/*mam       110*/
          nullify (new_array)                                                   !/*mam       111*/
        else                                                                    !/*mam       112*/
          new_array=0                                                           !/*mam       113*/
          if (associated(in_array)) then                                        !/*mam       114*/
            n1=size(in_array)                                                   !/*mam       115*/
            n1=min(n1,nl1)                                                      !/*mam       116*/
            new_array(1:n1)=in_array                                            !/*mam       117*/
            deallocate (in_array,stat=ist)                                      !/*mam       118*/
          endif                                                                 !/*mam       119*/
          if (ist == 0) then                                                    !/*mam       120*/
            in_array => new_array                                               !/*mam       121*/
          else                                                                  !/*mam       122*/
            deallocate (new_array)                                              !/*mam       123*/
            nullify (new_array)                                                 !/*mam       124*/
          endif                                                                 !/*mam       125*/
        endif                                                                   !/*mam       126*/
        return                                                                  !/*mam       127*/
        end function chg_d                                                      !/*mam       128*/
      end module allocate_memory                                                !/*mam       129*/
