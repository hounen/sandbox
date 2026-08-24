!#ident bnsrch.F from deck xbs                                                /*xbs         1*/
      subroutine bnsrch(tv,il,ih,ig)                                            !/*xbs         2*/
!        find the value tv in the array slice xss(il..ih)                       !/*xbs         3*/
!                                                                               !/*xbs         4*/
!     Preconditions:                                                            !/*xbs         5*/
!        tv is a real value within the array                                    !/*xbs         6*/
!        xss(il..ih) is an numerically ascending ordered array list             !/*xbs         7*/
!        il is the first word of the list (index low)                           !/*xbs         8*/
!        ih is the last word of the list (index high)                           !/*xbs         9*/
!                                                                               !/*xbs        10*/
!        tv and all xss(i) are read only variables                              !/*xbs        11*/
!                                                                               !/*xbs        12*/
!     Postconditions:                                                           !/*xbs        13*/
!        il/ih bracket the test value tv such that ih-il.eq.1 and               !/*xbs        14*/
!          xss(il) < tv < xss(ih)                                               !/*xbs        15*/
!                                                                               !/*xbs        16*/
!        Occasionally a value outside the array will be called for              !/*xbs        17*/
!        and a reasonable value, the extreme edge of the array,                 !/*xbs        18*/
!        will be returned along with an non-zero error flag ig                  !/*xbs        19*/
!          ig =  0 is normal state - value found in array slice                 !/*xbs        20*/
!          ig = -1 is warning state - value below first array value             !/*xbs        21*/
!          ig =  1 is warning state - value above last array value              !/*xbs        22*/
!          ig =  2 is warning state - incoming indices equal tv=xss(il)         !/*xbs        23*/
!          ig = -3 is warning state - incoming indices equal tv<xss(il)         !/*xbs        24*/
!          ig =  3 is warning state - incoming indices equal tv>xss(il)         !/*xbs        25*/
!        also il = ih = edge value for exit warning condition                   !/*xbs        26*/
!                                                                               !/*xbs        27*/
!        il, ih and ig are modified return values                               !/*xbs        28*/
!                                                                               !/*xbs        29*/
      use global_data                                                           !/*xbs        30*/
!                                                                               !/*xbs        31*/
      implicit real(kindr) (a-h,o-z)                                            !/*xbs        32*/
      implicit integer(kindi) (i-n)                                             !/*xbs        33*/
!                                                                               !/*xbs        34*/
!                                                                               !/*xbs        35*/
!        warning state - value below table                                      !/*xbs        36*/
      if(tv.lt.xss(il))then                                                     !/*xbs        37*/
         ig=-1                                                                  !/*xbs        38*/
         ih=il                                                                  !/*xbs        39*/
!                                                                               !/*xbs        40*/
!        warning state - value above table                                      !/*xbs        41*/
      else if(tv.gt.xss(ih))then                                                !/*xbs        42*/
         ig=1                                                                   !/*xbs        43*/
         il=ih                                                                  !/*xbs        44*/
!                                                                               !/*xbs        45*/
!        warning state - incoming indices are equal                             !/*xbs        46*/
      else if(il.eq.ih)then                                                     !/*xbs        47*/
         if(tv.eq.xss(il))then                                                  !/*xbs        48*/
            ig=2                                                                !/*xbs        49*/
         else if(tv.lt.xss(il))then                                             !/*xbs        50*/
            ig=-3                                                               !/*xbs        51*/
         else                                                                   !/*xbs        52*/
            ig=3                                                                !/*xbs        53*/
         endif                                                                  !/*xbs        54*/
!                                                                               !/*xbs        55*/
!        else handle normal value within table                                  !/*xbs        56*/
      else                                                                      !/*xbs        57*/
         ig=0                                                                   !/*xbs        58*/
 10      if(ih-il.eq.1)return                                                   !/*xbs        59*/
         im=(il+ih)/2                                                           !/*xbs        60*/
         if(tv.lt.xss(im))go to 20                                              !/*xbs        61*/
         il=im                                                                  !/*xbs        62*/
         go to 10                                                               !/*xbs        63*/
 20      ih=im                                                                  !/*xbs        64*/
         go to 10                                                               !/*xbs        65*/
!                                                                               !/*xbs        66*/
!        end of cases                                                           !/*xbs        67*/
      endif                                                                     !/*xbs        68*/
      return                                                                    !/*xbs        69*/
      end                                                                       !/*xbs        70*/
