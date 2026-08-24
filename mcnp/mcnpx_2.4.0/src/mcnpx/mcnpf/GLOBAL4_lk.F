c#ident src/mcnpx/mcnpf/GLOBAL4_lk.F from deck mth                            /*mth         1*/
      module thread                                                             !/*mth         2*/
!        module for threading - includes locks, barriers, and                   !/*mth         3*/
!        parallel execution.                                                    !/*mth         4*/
!        follows openmp standard (www.openmp.org).                              !/*mth         5*/
      use global_parameters                                                     !/*mth         6*/
      implicit none                                                             !/*mth         7*/
      save                                                                      !/*mth         8*/
      public                                                                    !/*mth         9*/
#ifdef MULTT                                                                    /*!mth        10*/
      integer(4), private, parameter :: lockoff = -1                            !/*mth        11*/
      integer(4), private, parameter :: lockon  =  1                            !/*mth        12*/
      integer(4), private:: lock_key                                            !/*mth        13*/
      real(kindr), private:: lock_cpu = 0.                                      !/*mth        14*/
      real(kindr), private:: barr_cpu = 0.                                      !/*mth        15*/
      real, external:: wclock                                                   !/*mth        16*/
      contains                                                                  !/*mth        17*/
!                                                                               !/*mth        18*/
      subroutine thread_init                                                    !/*mth        19*/
!        initialize thread lock.                                                !/*mth        20*/
      call omp_init_lock(lock_key)                                              !/*mth        21*/
      return                                                                    !/*mth        22*/
      end subroutine thread_init                                                !/*mth        23*/
!                                                                               !/*mth        24*/
      subroutine thread_stop                                                    !/*mth        25*/
!        destroy thread lock.                                                   !/*mth        26*/
      call omp_destroy_lock(lock_key)                                           !/*mth        27*/
      return                                                                    !/*mth        28*/
      end subroutine thread_stop                                                !/*mth        29*/
!                                                                               !/*mth        30*/
      subroutine thread_lockon(lock_state)                                      !/*mth        31*/
!        set shared-memory lock - nested locks increment lock_state.            !/*mth        32*/
      integer(kindi), intent(inout):: lock_state                                !/*mth        33*/
      real(kindr) :: wc                                                         !/*mth        34*/
!                                                                               !/*mth        35*/
      wc=wclock()                                                               !/*mth        36*/
      select case (lock_state)                                                  !/*mth        37*/
        case (lockoff)                                                          !/*mth        38*/
          call omp_set_lock(lock_key)                                           !/*mth        39*/
          lock_state=lockon                                                     !/*mth        40*/
        case (lockon)                                                           !/*mth        41*/
          lock_state=lockon+1                                                   !/*mth        42*/
      end select                                                                !/*mth        43*/
      lock_cpu=lock_cpu+wclock()-wc                                             !/*mth        44*/
      return                                                                    !/*mth        45*/
      end subroutine thread_lockon                                              !/*mth        46*/
!                                                                               !/*mth        47*/
      subroutine thread_lockoff(lock_state)                                     !/*mth        48*/
!        release shared-memory lock - nested locks decrement lock_state.        !/*mth        49*/
      integer(kindi),intent(inout):: lock_state                                 !/*mth        50*/
!                                                                               !/*mth        51*/
      select case (lock_state)                                                  !/*mth        52*/
        case (lockon)                                                           !/*mth        53*/
          call omp_unset_lock(lock_key)                                         !/*mth        54*/
          lock_state=lockoff                                                    !/*mth        55*/
        case (lockon+1)                                                         !/*mth        56*/
          lock_state=lockon                                                     !/*mth        57*/
      end select                                                                !/*mth        58*/
      return                                                                    !/*mth        59*/
      end subroutine thread_lockoff                                             !/*mth        60*/
!                                                                               !/*mth        61*/
      subroutine thread_barrier                                                 !/*mth        62*/
!        set a thread barrier.                                                  !/*mth        63*/
      real(kindr) :: wc                                                         !/*mth        64*/
!                                                                               !/*mth        65*/
      wc=wclock()                                                               !/*mth        66*/
!$OMP BARRIER                                                                   !/*mth        67*/
      barr_cpu=barr_cpu+wclock()-wc                                             !/*mth        68*/
      return                                                                    !/*mth        69*/
      end subroutine thread_barrier                                             !/*mth        70*/
!                                                                               !/*mth        71*/
      subroutine thread_id(mt)                                                  !/*mth        72*/
!        get the thread number of the calling thread.                           !/*mth        73*/
      integer(kindi), intent(out):: mt                                          !/*mth        74*/
      integer(4), external :: omp_get_thread_num                                !/*mth        75*/
!                                                                               !/*mth        76*/
      mt = omp_get_thread_num()                                                 !/*mth        77*/
      return                                                                    !/*mth        78*/
      end subroutine thread_id                                                  !/*mth        79*/
!                                                                               !/*mth        80*/
      subroutine thread_number(nt)                                              !/*mth        81*/
!        set the number of threads.                                             !/*mth        82*/
      integer(kindi), intent(in) :: nt                                          !/*mth        83*/
      integer(4) :: n                                                           !/*mth        84*/
!                                                                               !/*mth        85*/
      n=nt                                                                      !/*mth        86*/
      call omp_set_num_threads(n)                                               !/*mth        87*/
      return                                                                    !/*mth        88*/
      end subroutine thread_number                                              !/*mth        89*/
!                                                                               !/*mth        90*/
      subroutine thread_routine(name)                                           !/*mth        91*/
!        parallel subroutine call.                                              !/*mth        92*/
      external name                                                             !/*mth        93*/
!$OMP PARALLEL SHARED(name)                                                     !/*mth        94*/
      call name                                                                 !/*mth        95*/
!$OMP END PARALLEL                                                              !/*mth        96*/
      return                                                                    !/*mth        97*/
      end subroutine thread_routine                                             !/*mth        98*/
!                                                                               !/*mth        99*/
      function thread_ltime() result(time)                                      !/*mth       100*/
!        provide total time waiting for locks.                                  !/*mth       101*/
      real(kindr) :: time                                                       !/*mth       102*/
!                                                                               !/*mth       103*/
!                                                                               !/*mth       104*/
      time=lock_cpu                                                             !/*mth       105*/
      return                                                                    !/*mth       106*/
      end function thread_ltime                                                 !/*mth       107*/
!                                                                               !/*mth       108*/
      function thread_btime() result(time)                                      !/*mth       109*/
!        provide total time waiting for barriers.                               !/*mth       110*/
      real(kindr) :: time                                                       !/*mth       111*/
!                                                                               !/*mth       112*/
      time=barr_cpu                                                             !/*mth       113*/
      return                                                                    !/*mth       114*/
      end function thread_btime                                                 !/*mth       115*/
#endif                                                                          /*!mth       116*/
!                                                                               !/*mth       117*/
      end module thread                                                         !/*mth       118*/
