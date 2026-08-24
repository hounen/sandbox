!#ident rang.F from deck rf                                                     !/*rf          1*/
      function rang()                                                           !/*rf          2*/
!        return the next pseudo-random number.                                  !/*rf          3*/
      use global_data                                                           !/*rfx         1*/
!                                                                               !/*rfx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*rfx         3*/
      implicit integer(kindi) (i-n)                                             !/*rfx         4*/
!                                                                               !/*rfx         5*/
      parameter (p=2d0**24,q=2d0**(-24),r=2d0**(-48))                           !/*rf          7*/
!                                                                               !/*rf          8*/
!        rang()=mod(2**48*rang()*rnmult,2**48)                                  !/*rf          9*/
!        split rang() and rnmult into upper and lower 24-bit halves,            !/*rf         10*/
!        ranb,rans,rngb,rngs, respectively, to achieve 96-bit precision.        !/*rf         11*/
!        this expression for b is invalid unless rngb+rngs < 2**24              !/*rf         12*/
!        or unless the more elaborate expression of advijk is used.             !/*rf         13*/
      a=rngs*rans                                                               !/*rf         14*/
      b=rngb*rans+rngs*ranb+aint(a*q)                                           !/*rf         15*/
      rans=a-aint(a*q)*p                                                        !/*rf         16*/
      ranb=b-aint(b*q)*p                                                        !/*rf         17*/
      rang=(ranb*p+rans)*r                                                      !/*rf         18*/
      rnrtc=rnrtc+1.                                                            !/*rf         19*/
      return                                                                    !/*rf         20*/
      end                                                                       !/*rf         21*/
