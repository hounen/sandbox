      implicit real*8(a-h,o-y)
      implicit complex*16(z)
      parameter(nd=200,pi=3.141592653589793d0,zi=(0.d0,1.d0)
     &         ,hbc=197.327053d0,al=1.d0/137.0359895d0
     &         ,dmass=938.91897d0)
      
      common/fact/fac(0:20),dfac(0:20)
