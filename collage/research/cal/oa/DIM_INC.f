      implicit real*8 (a-h,o-y)
      implicit complex*16 (z)
      character*80 memo
      parameter(ntot=100,pi=3.141592653589793d0
     &         ,hbc=197.327953d0,alpha=1.d0/137.0359895d0
     &         ,dmass=938.91897d0)
      common /chdata/ltot,nb,db0,dgam
      common /mat/za(ntot),ze(ntot),dn(ntot,ntot),zh(ntot,ntot)
     &       ,ze0(ntot),dn0(ntot,ntot),zh0(ntot,ntot) 
      common /fact/fac(0:20),faclog(0:30),dfaclog(0:30)

