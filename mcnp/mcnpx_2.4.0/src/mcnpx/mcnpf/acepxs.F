!#ident acepxs.F from deck xap                                                /*xap         1*/
      function acepxs(it,kx,r,nt,lb)                                            !/*xap         2*/
!        interpolate value at index kx in table at xss(it)                      !/*xap         3*/
!                                                                               !/*xap         4*/
!        Preconditions:                                                         !/*xap         5*/
!                                                                               !/*xap         6*/
!          xss(it) is the first word of an appropriate table                    !/*xap         7*/
!                                                                               !/*xap         8*/
!          kx is the current index in the table                                 !/*xap         9*/
!          r is the linear interpolation factor, 0.0 to 1.0,                    !/*xap        10*/
!            between values at index kx and kx+1                                !/*xap        11*/
!                                                                               !/*xap        12*/
!          table data are in the format:                                        !/*xap        13*/
!            xss(it)              = ie - index of first value                   !/*xap        14*/
!            xss(it+1)            = nv - number of values given                 !/*xap        15*/
!              ie+nv-1            = il - index of last value                    !/*xap        16*/
!            xss(it+2..it+2+nv-1) = values(ie..il)                              !/*xap        17*/
!            values are interpolated lin-lin from the current table             !/*xap        18*/
!                                                                               !/*xap        19*/
!            nt is interpolation scheme for an index out of bounds:             !/*xap        20*/
!              (index out of bounds is kx < ie or kx > il)                      !/*xap        21*/
!              nt =  0 - return zero for no value out of energy bounds          !/*xap        22*/
!              nt =  1 - return end point value for energy out of bounds        !/*xap        23*/
!              nt = -1 - return zero below and end point above bounds           !/*xap        24*/
!                                                                               !/*xap        25*/
!          it, kx, r, nt and all xss(i) are read only variables                 !/*xap        26*/
!                                                                               !/*xap        27*/
!                                                                               !/*xap        28*/
!        Postconditions:                                                        !/*xap        29*/
!                                                                               !/*xap        30*/
!          returns value acepxs(kx) appropriately interpolated                  !/*xap        31*/
!                                                                               !/*xap        32*/
!          returns lb for index error checking:                                 !/*xap        33*/
!            lb =  0 - value interpolated within table bounds                   !/*xap        34*/
!            lb =  1 - value above table index bound                            !/*xap        35*/
!            lb = -1 - value below table index bound                            !/*xap        36*/
!                                                                               !/*xap        37*/
!          acepxs and lb are modified return values                             !/*xap        38*/
!                                                                               !/*xap        39*/
      use global_data                                                           !/*xap        40*/
!                                                                               !/*xap        41*/
      implicit real(kindr) (a-h,o-z)                                            !/*xap        42*/
      implicit integer(kindi) (i-n)                                             !/*xap        43*/
!                                                                               !/*xap        44*/
!                                                                               !/*xap        45*/
!        default return value is zero                                           !/*xap        46*/
      acepxs=0                                                                  !/*xap        47*/
!                                                                               !/*xap        48*/
!        find table bounds and check incoming index                             !/*xap        49*/
      ie=nint(xss(it))                                                          !/*xap        50*/
      nv=nint(xss(it+1))                                                        !/*xap        51*/
      il=ie+nv-1                                                                !/*xap        52*/
!                                                                               !/*xap        53*/
!        if index below table, return appropriate value                         !/*xap        54*/
      if (kx.lt.ie) then                                                        !/*xap        55*/
         if (nt.eq.1) then                                                      !/*xap        56*/
            acepxs=xss(it+2)                                                    !/*xap        57*/
         endif                                                                  !/*xap        58*/
         lb=-1                                                                  !/*xap        59*/
         return                                                                 !/*xap        60*/
      endif                                                                     !/*xap        61*/
!                                                                               !/*xap        62*/
!       if index above table, return appropriate value                          !/*xap        63*/
      if (kx.gt.il .or. (kx.eq.il .and. r.gt.0)) then                           !/*xap        64*/
         if (nt.eq.1 .or. nt.eq.-1) then                                        !/*xap        65*/
            acepxs=xss(it+2+nv-1)                                               !/*xap        66*/
         endif                                                                  !/*xap        67*/
         lb=1                                                                   !/*xap        68*/
         return                                                                 !/*xap        69*/
      endif                                                                     !/*xap        70*/
!                                                                               !/*xap        71*/
!       else interpolate the appropriate value from the table                   !/*xap        72*/
      if (r.eq.0.) then                                                         !/*xap        73*/
         acepxs=xss(it+2+kx-ie)                                                 !/*xap        74*/
      else                                                                      !/*xap        75*/
         acepxs=xss(it+2+kx-ie)+r*(xss(it+2+kx-ie+1)-xss(it+2+kx-ie))           !/*xap        76*/
      endif                                                                     !/*xap        77*/
      lb=0                                                                      !/*xap        78*/
      return                                                                    !/*xap        79*/
      end                                                                       !/*xap        80*/
