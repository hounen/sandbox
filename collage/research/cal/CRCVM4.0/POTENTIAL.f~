C==== SET POTENTIAL ====================================================

      SUBROUTINE SET_POT

C-----------------------------------------------------------------------
      INCLUDE 'DIM_INC.f'
      INCLUDE 'GEM_INC.f'
      DIMENSION VTMP(3),DWC(3),DBC(3),DHC(3),DMC(3)
      IF(IS_NN.EQ.1) THEN
C
C SET MINNESOTA FORCE
C
        NC_NN=3
        NLS_NN=0
        NTN_NN=0
        U2=0.5D0*U_PARA
        DO I=0,1
        DO J=0,1
          RC_NN(1,I,J)=1.487D0
          RC_NN(2,I,J)=0.639D0
          RC_NN(3,I,J)=0.465D0
        ENDDO
        ENDDO
        VTMP(1)= 200.0D0
        VTMP(2)=-178.0D0
        VTMP(3)=-91.85D0

        DWC(1)= VTMP(1)*U2
        DWC(2)= VTMP(2)*0.5D0*U2
        DWC(3)= VTMP(3)*0.5D0*U2

        DBC(1)= 0.D0
        DBC(2)= VTMP(2)*0.5D0*U2
        DBC(3)=-VTMP(3)*0.5D0*U2

        DHC(1)= 0.D0
        DHC(2)= VTMP(2)*(1.D0-U2)*0.5D0
        DHC(3)=-VTMP(3)*(1.D0-U2)*0.5D0

        DMC(1)= VTMP(1)*(1.D0-U2)
        DMC(2)= VTMP(2)*(1.D0-U2)*0.5D0
        DMC(3)= VTMP(3)*(1.D0-U2)*0.5D0
        DO I=1,3
          VC_NN(I,0,0)=DWC(I)-DBC(I)+DHC(I)-DMC(I)
          VC_NN(I,1,0)=DWC(I)+DBC(I)+DHC(I)+DMC(I)
          VC_NN(I,0,1)=DWC(I)-DBC(I)-DHC(I)+DMC(I)
          VC_NN(I,1,1)=DWC(I)+DBC(I)-DHC(I)-DMC(I)
        ENDDO
      ELSEIF(IS_NN.EQ.2) THEN
C
C SET AV8' FORCE
C
        NC_NN=6
        NLS_NN=6
        NTN_NN=8

        VC_NN(1,0,0)=  -5.3718485D0
        VC_NN(2,0,0)=  42.0351149D0
        VC_NN(3,0,0)=1313.1214374D0
        VC_NN(4,0,0)= 156.8939167D0
        VC_NN(5,0,0)=   1.6440785D0
        VC_NN(6,0,0)=   6.1188200D0
        RC_NN(1,0,0)=1.D0/0.1658676D0**2.D0
        RC_NN(2,0,0)=1.D0/0.2894831D0**2.D0
        RC_NN(3,0,0)=1.D0/0.5052249D0**2.D0
        RC_NN(4,0,0)=1.D0/0.8817517D0**2.D0
        RC_NN(5,0,0)=1.D0/1.5388910D0**2.D0
        RC_NN(6,0,0)=1.D0/2.6857737D0**2.D0

        VC_NN(1,1,0)=  91.4822846D0
        VC_NN(2,1,0)=2822.2369593D0
        VC_NN(3,1,0)=-467.7657278D0
        VC_NN(4,1,0)= -19.8229349D0
        VC_NN(5,1,0)= -16.3953104D0
        VC_NN(6,1,0)=  -3.5568691D0
        RC_NN(1,1,0)=1.D0/0.2844815D0**2.D0
        RC_NN(2,1,0)=1.D0/0.4299440D0**2.D0
        RC_NN(3,1,0)=1.D0/0.6497851D0**2.D0
        RC_NN(4,1,0)=1.D0/0.9820364D0**2.D0
        RC_NN(5,1,0)=1.D0/1.4841760D0**2.D0
        RC_NN(6,1,0)=1.D0/2.2430721D0**2.D0

        VC_NN(1,0,1)= 118.1762345D0
        VC_NN(2,0,1)=3754.5356911D0
        VC_NN(3,0,1)=-757.3370635D0
        VC_NN(4,0,1)= -68.1523604D0
        VC_NN(5,0,1)= -21.2313598D0
        VC_NN(6,0,1)=  -3.8928738D0
        RC_NN(1,0,1)=1.D0/0.2864738D0**2.D0
        RC_NN(2,0,1)=1.D0/0.4310967D0**2.D0
        RC_NN(3,0,1)=1.D0/0.6487308D0**2.D0
        RC_NN(4,0,1)=1.D0/0.9762350D0**2.D0
        RC_NN(5,0,1)=1.D0/1.4690757D0**2.D0
        RC_NN(6,0,1)=1.D0/2.2107213D0**2.D0

        VC_NN(1,1,1)= 133.3890061D0
        VC_NN(2,1,1)=1277.2251624D0
        VC_NN(3,1,1)=1481.3409438D0
        VC_NN(4,1,1)=-929.4184626D0
        VC_NN(5,1,1)= 474.6536455D0
        VC_NN(6,1,1)=-121.0755683D0
        RC_NN(1,1,1)=1.D0/0.3410784D0**2.D0
        RC_NN(2,1,1)=1.D0/0.4233829D0**2.D0
        RC_NN(3,1,1)=1.D0/0.5255481D0**2.D0
        RC_NN(4,1,1)=1.D0/0.6523664D0**2.D0
        RC_NN(5,1,1)=1.D0/0.8097869D0**2.D0
        RC_NN(6,1,1)=1.D0/1.0051941D0**2.D0

        VLS_NN(1,1,0)=  4.7149736D0
        VLS_NN(2,1,0)=-92.5679591D0
        VLS_NN(3,1,0)=301.5818757D0
        VLS_NN(4,1,0)=  1.7134339D0
        VLS_NN(5,1,0)= -0.8889886D0
        VLS_NN(6,1,0)=  0.0930875D0
        RLS_NN(1,1,0)=1.D0/0.1975533D0**2.D0
        RLS_NN(2,1,0)=1.D0/0.3369489D0**2.D0
        RLS_NN(3,1,0)=1.D0/0.5747036D0**2.D0
        RLS_NN(4,1,0)=1.D0/0.9802204D0**2.D0
        RLS_NN(5,1,0)=1.D0/1.6718741D0**2.D0
        RLS_NN(6,1,0)=1.D0/2.8515658D0**2.D0

        VLS_NN(1,1,1)= -20.6687445D0
        VLS_NN(2,1,1)= 406.4188719D0
        VLS_NN(3,1,1)=-902.2225521D0
        VLS_NN(4,1,1)= -18.4134186D0
        VLS_NN(5,1,1)=   1.3631363D0
        VLS_NN(6,1,1)=  -0.1837394D0
        RLS_NN(1,1,1)=1.D0/0.2016024D0**2.D0
        RLS_NN(2,1,1)=1.D0/0.3474255D0**2.D0
        RLS_NN(3,1,1)=1.D0/0.5987251D0**2.D0
        RLS_NN(4,1,1)=1.D0/1.0317948D0**2.D0
        RLS_NN(5,1,1)=1.D0/1.7781124D0**2.D0
        RLS_NN(6,1,1)=1.D0/3.0642562D0**2.D0

        VTN_NN(1,1,0)=-8031.8094845D0
        VTN_NN(2,1,0)=-3397.2153441D0
        VTN_NN(3,1,0)=-2180.2316049D0
        VTN_NN(4,1,0)= -420.7351591D0
        VTN_NN(5,1,0)= -214.2512043D0
        VTN_NN(6,1,0)=  -17.9748552D0
        VTN_NN(7,1,0)=   -1.1514334D0
        VTN_NN(8,1,0)=   -0.0483353D0
        RTN_NN(1,1,0)=1.D0/0.1057489D0**2.D0
        RTN_NN(2,1,0)=1.D0/0.1728539D0**2.D0
        RTN_NN(3,1,0)=1.D0/0.2825416D0**2.D0
        RTN_NN(4,1,0)=1.D0/0.4618338D0**2.D0
        RTN_NN(5,1,0)=1.D0/0.7548994D0**2.D0
        RTN_NN(6,1,0)=1.D0/1.2339352D0**2.D0
        RTN_NN(7,1,0)=1.D0/2.0169525D0**2.D0
        RTN_NN(8,1,0)=1.D0/3.2968485D0**2.D0

        VTN_NN(1,1,1)=-486.2603768D0
        VTN_NN(2,1,1)= 318.9744930D0
        VTN_NN(3,1,1)= 661.1064012D0
        VTN_NN(4,1,1)=  26.1396422D0
        VTN_NN(5,1,1)=   3.8691045D0
        VTN_NN(6,1,1)=   0.2348827D0
        VTN_NN(7,1,1)=0.D0
        VTN_NN(8,1,1)=0.D0
        RTN_NN(1,1,1)=1.D0/0.1678926D0**2.D0
        RTN_NN(2,1,1)=1.D0/0.2829651D0**2.D0
        RTN_NN(3,1,1)=1.D0/0.4769077D0**2.D0
        RTN_NN(4,1,1)=1.D0/0.8037774D0**2.D0
        RTN_NN(5,1,1)=1.D0/1.3546817D0**2.D0
        RTN_NN(6,1,1)=1.D0/2.2831723D0**2.D0
        RTN_NN(7,1,1)=1.D0
        RTN_NN(8,1,1)=1.D0

        IF(IS_LS.NE.1) NLS_NN=0
        IF(IS_TN.NE.1) NTN_NN=0
      ENDIF
      IF(IS_CN.NE.1) THEN
C
C SET KKNN POTENTIAL
C
        NC_CN=2
        NCP_CN=3
        NLS_CN=3
        NLSP_CN=2

        VC_CN(1)=-96.3D0
        VC_CN(2)= 77.0D0
        RC_CN(1)= 0.36D0
        RC_CN(2)= 0.90D0

        VCP_CN(1)= 34.0D0
        VCP_CN(2)=-85.0D0
        VCP_CN(3)= 51.0D0
        RCP_CN(1)= 0.20D0
        RCP_CN(2)= 0.53D0
        RCP_CN(3)= 2.50D0

        VLS_CN(1)=-16.8D0
        VLS_CN(2)=-20.0D0
        VLS_CN(3)= 20.0D0
        RLS_CN(1)= 0.52D0
        RLS_CN(2)= 0.396D0
        RLS_CN(3)= 2.20D0

        VLSP_CN(1)=-20.0D0*(-0.3D0)
        VLSP_CN(2)= 20.0D0*(-0.3D0)
        RLSP_CN(1)= 0.396D0
        RLSP_CN(2)= 2.20D0
      ELSEIF(IS_CN.EQ.1) THEN
C
C SET ONE GAUSS CENTRAL POTENTIAL
C
        NC_CN=1
        NCP_CN=0
        NLS_CN=0
        NLSP_CN=0
        VC_CN(1)=-53.2D0
        RC_CN(1)=0.195D0
      ENDIF
C
C SET SCHMIDT-WILDERMUTH POTENTIAL
C
      XW=0.4075D0
      XM=0.4075D0
      XB=0.0925D0
      XH=0.0925D0
      XVC=-72.98D0
      XMU=0.4600D0
      XNU=0.2675D0

      VC_AA=2.D0*XVC*(8.D0*XW+4.D0*XB-4.D0*XH-2.D0*XM)
     &     *DSQRT(4.D0*XNU/(4.D0*XNU+3.D0*XMU))**3.D0
      VC_AA=VC_AA*1.03D0
      RC_AA=4.D0*XNU*XMU/(4.D0*XNU+3.D0*XMU)
C
C SET ALPHA RADIUS
C
      CORE_RAD=1.4D0
      RETURN
      END
C=======================================================================
