C KKNN POTENTIAL 

C CHANNEL RANGE MATRIX ELEMENT CAL.

      SUBROUTINE KKNN(ZH,DL,DJ,ZA1,ZA2) 
      INCLUDE 'LI_INC.f'
      ZCT=(0.D0,0.D0)
      ZCL=(0.D0,0.D0)
      ZCLS=(0.D0,0.D0)
      CALL KINE(ZT,DL,ZA1,ZA2)
      CALL CENT(ZCT,DL,ZA1,ZA2)
      CALL CENL(ZCL,DL,ZA1,ZA2)
      CALL LSFRS(ZCLS,DL,DJ,ZA1,ZA2)
      CALL PSEUDO(ZPF,DL,ZA1,ZA2)
      ZH=ZT+ZCT+ZCL+ZCLS!+ZPF
      RETURN
      END
      
C ASYMPTOTIC MATRIX ELEMENT CAL.

      SUBROUTINE KKNN0(ZH0,DL,ZA1,ZA2) 
      INCLUDE 'LI_INC.f'
      CALL KINE(ZT,DL,ZA1,ZA2)
      ZH0=ZT
      RETURN
      END

C SBB POTENTIAL

C CHANNEL RANGE MATRIX ELEMENT CAL.

      SUBROUTINE SBB(ZWH,DL,DJ,ZA1,ZA2,IME,DIS)           
      INCLUDE 'LI_INC.f'
      ZWCT=(0.D0,0.D0)
      ZWLS=(0.D0,0.D0)
      ZPF =(0.D0,0.D0)
      CALL WKINE(ZWT,DL,ZA1,ZA2)
      CALL WCENT(ZWCT,DL,ZA1,ZA2,IME,DIS)
      CALL WLSFRS2(ZWLS,DL,DJ,ZA1,ZA2,IME,DIS)
      CALL PSEUDO(ZPF,DL,ZA1,ZA2)
      ZWH=ZWT+ZWCT+ZWLS+ZPF
      RETURN
      END 

C ASYMPTOTIC RANGE MATRIX ELEMENT CAL.

      SUBROUTINE SBB0(ZWH0,DL,ZA1,ZA2)           
      INCLUDE 'LI_INC.f'
      CALL WKINE(ZWT,DL,ZA1,ZA2)
      ZWH0=ZWT
      RETURN
      END
