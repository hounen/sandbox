!     $Id: xstalw.F
      subroutine xstalw (iut,tlly,tlsq,ndim1,ndim2,ergb,xmu,nc)
!        Write tally data to XSTAL in MCTAL format.
#include <htape3x/com32b.h>
#include <htape3x/label.h>
#include <htape3x/contrl.h>
#include <xsex3/charxs.h>
      dimension tlly(ndim1,ndim2,1),tlsq(ndim1,ndim2,1),ergb(1),xmu(1)
      character title*33
!        General tally header
      do 40 k=1,ntype
      if(tlly(ndim1,ndim2,k).le.dp0) go to 40
!        Particle section header
      write(iut,'(a5,2i5)')'tally',100*nc+k,0
      if (k.eq.lpt1) then
        if (iyield.eq.0) then
          title=kpart(k)//' scattering cross section'
        else
          title=kpart(k)//' scattering distribution '
        endif
      else
        if (iyield.eq.0) then
          title=kpart(k)//' production cross section'
        else
          title=kpart(k)//' production yield        '
        endif
      endif
      write(iut,'(5x,a33,42x)')title
!        Parameter specs
      write(iut,'(a1,i9)')'d',1
      write(iut,'(a1,i9)')'u',0
      write(iut,'(a1,i9)')'s',0
      write(iut,'(a1,i9)')'m',0
      if (l_deg) then
        write(iut,'(a2,i8)')'ct',ndim1+1
        write(iut,'(1p6e13.5)')(s_ang(xmu(1),i,ndim1),i=1,ndim1)
      else
        write(iut,'(a2,i8)')'ct',ndim1
        write(iut,'(1p6e13.5)')(xmu(i),i=1,ndim1-1)
      endif
      write(iut,'(a2,i8)')'et',ndim2-1
      write(iut,'(1p6e13.5)')(ergb(i),i=1,ndim2-2)
      write(iut,'(a1,i9)')'t',0
!        Data section for 1 particle type
      write(iut,'(a4)')'vals'
      if (l_deg) then
        write(iut,'(4(1pe13.5,0pf7.4))')((s_val(tlly(1,j,k),i,ndim1),
     &  s_val(tlsq(1,j,k),i,ndim1),j=1,ndim2-2),s_val(tlly(1,ndim2,k),
     &  i,ndim1),s_val(tlsq(1,ndim2,k),i,ndim1),i=1,ndim1+1)
      else
        write(iut,'(4(1pe13.5,0pf7.4))')((tlly(i,j,k),tlsq(i,j,k),j=1,
     &  ndim2-2),tlly(i,ndim2,k),tlsq(i,ndim2,k),i=1,ndim1)
      endif
      write(iut,'(a3,i5,8i8)') 'tfc',0,0,0,0,0,0,0,0,0
   40 continue
      return
      end
      function s_ang (xmu,i,ndim1)
#include <htape3x/com32b.h>
      dimension xmu(1)
      if (i.eq.1) then
        s_ang=180.d0
      else
        s_ang=180.d0*acos(xmu(i-1))/dppi
      endif
      return
      end
      function s_val (val,i,ndim1)
#include <htape3x/com32b.h>
      dimension val(1)
      if (i.eq.1) then
        s_val=dp0
      elseif (i.le.ndim1) then
        s_val=val(i-1)
      else
        s_val=val(ndim1)
      endif
      return
      end
