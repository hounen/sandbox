!     $Id: nizl.F,v 1.12 2002/03/27 18:14:46 gwm Exp $                        !/*ni          1*/
      subroutine nizl (it,aaa,po,si,zl)                                         !/*ni          2*/
!                                                                     *         !/*ni          3*/
!---------------------------------------------------------------------*         !/*ni          4*/
!                                                                     *         !/*ni          5*/
#include <lcs/double.h>                                                         /*!ni          6*/
#include <fluka89/flprm1.h>                                                     /*!ni          7*/
#include <fluka89/flprm2.h>                                                     /*!ni          8*/
#include <lcs/inout.h>                                                          /*!ni          9*/
!********************************************************************           !/*ni         10*/
!     Modification by R. E. Prael 10/02/2000 inserting A. Ferrari               !/*ni         11*/
!     changes from FLUKA96 removing 1.07 factor in neutron cross                !/*ni         12*/
!     sections (lines commented out, noted CAF).                                !/*ni         13*/
!     version october 83 by          h. moehring                                !/*ni         14*/
!                                    leipzig                                    !/*ni         15*/
!     last change 24. april   -86 by jukka lindgren                             !/*ni         16*/
!                                    helsinki university of                     !/*ni         17*/
!                                    technology, finland                        !/*ni         18*/
!                                                                               !/*ni         19*/
!     see: h.j. moehring, hadron-nucleus inelastic cross-sections for           !/*ni         20*/
!     use in hadron-cascade calculations at high energies,                      !/*ni         21*/
!     tis division report 14. october 1983, tis-rp/116, cern geneva             !/*ni         22*/
!                                                                               !/*ni         23*/
!     this is a subroutine of fluka82 to calculate the inelastic                !/*ni         24*/
!     scattering length of the material in g/cm**2.                             !/*ni         25*/
!                                                                               !/*ni         26*/
!     input variables:                                                          !/*ni         27*/
!        it     = type of the particle                                          !/*ni         28*/
!        aa     = atomic weight of the nucleus                                  !/*ni         29*/
!        po     = particle momentum in gev/c                                    !/*ni         30*/
!                                                                               !/*ni         31*/
!     output variables:                                                         !/*ni         32*/
!        si     = interpolated cross section in millibarns                      !/*ni         33*/
!        zl     = interpolated absorption length in g/cm**2                     !/*ni         34*/
!                                                                               !/*ni         35*/
!     other inportant variables:                                                !/*ni         36*/
!        sig    = proton/nuclei cross sections                                  !/*ni         37*/
!        seg    = pion-/nuclei cross sections above 0.3 gev/c                   !/*ni         38*/
!        segp   = pion+/nuclei cross sections above 0.3 gev/c                   !/*ni         39*/
!        sigkm  = k+ and k0/nuclei cross sections above 3.0 gev/c               !/*ni         40*/
!        sigkp  = k+ and k0 bar/nuclei cross sections above 3.0 gev/c           !/*ni         41*/
!        sigap  = antinucleon/nuclei cross sections above 3.0 gev/c             !/*ni         42*/
!        seeg   = pion/nuclei cross sections below 0.3 gev/c                    !/*ni         43*/
!        p      = momentums for which the cross sections are given in           !/*ni         44*/
!                 sig, seg, segp, sigkm, sigkp and sigap                        !/*ni         45*/
!        pee    = momentums for which the cross sections are given in           !/*ni         46*/
!                 seeg                                                          !/*ni         47*/
!        a      = nuclei for which the cross sections are given in              !/*ni         48*/
!                 sig, seg, segp, sigkm, sigkp, sigap and seeg                  !/*ni         49*/
!        plab   = momentums for which the total cross sections are              !/*ni         50*/
!                 given in totcrs                                               !/*ni         51*/
!        totcrs = total cross sections as a function of momentum                !/*ni         52*/
!                 totcrs(k,i) where k=momentum index,i=reaction type            !/*ni         53*/
!                 i=1:negative kaon-proton  = kaon zero bar-neutron             !/*ni         54*/
!                 i=2:negative kaon-neutron = kaon zero bar-proton              !/*ni         55*/
!                 i=3:positive kaon-proton  = kaon zero neutron                 !/*ni         56*/
!                 i=4:positive kaon-neutron = kaon zero-proton                  !/*ni         57*/
!                 i=5:anti nucleon-nucleon                                      !/*ni         58*/
!                                                                               !/*ni         59*/
!     note1: presently cross sections are assumed to be constant                !/*ni         60*/
!     above 10000.0 gev/c for all particles and                                 !/*ni         61*/
!     below 0.13 gev/c for pions and below 0.3 gev/c for others                 !/*ni         62*/
!                                                                               !/*ni         63*/
!     note2: see table itt to find out how different hadrons                    !/*ni         64*/
!     are treated. all particles with particle number bigger than               !/*ni         65*/
!     25 are treated as protons.                                                !/*ni         66*/
!                                                                               !/*ni         67*/
!     note3: for leptons and photons practically zero cross section             !/*ni         68*/
!     is returned.                                                              !/*ni         69*/
!********************************************************************           !/*ni         70*/
!                                                                               !/*ni         71*/
!     dimension sig1(20,5),seg1(20,5),segp1(20,5),sig2(20,4),                   !/*ni         72*/
!    &seg2(20,4),segp2(20,4)                                                    !/*ni         73*/
!     equivalence (sig(1),sig1(1)),(sig(101),sig2(1)),                          !/*ni         74*/
!    &(seg(1),seg1(1)),(seg(101),seg2(1)),                                      !/*ni         75*/
!    &(segp(1),segp1(1)),(segp(101),segp2(1))                                   !/*ni         76*/
      dimension seeg(4,9), pee(4), sigkp(20,9), sigkm(20,9), sigap(20,9)        !/*ni         77*/
      dimension seg(20,9), sig(20,9), segp(20,9), p(20), a(9), itt(25)          !/*ni         78*/
      dimension bet(4), alph(4)                                                 !/*ni         79*/
      data alph /.748d0,.803d0,.63d0,.63d0/                                     !/*ni         80*/
      data bet /1.27d0,1.22d0,.9d0,.9d0/                                        !/*ni         81*/
      data a /1.d0,9.d0,12.d0,27.d0,55.9d0,63.5d0,112.4d0,207.2d0,238.1d        !/*ni         82*/
     & 0/                                                                       !/*ni         83*/
      data p /0.3d0,0.4d0,0.5d0,0.6d0,0.8d0,1.d0,1.5d0,2.d0,3.d0,4.d0,5         !/*ni         84*/
     & .d0,6.d0,10.d0,20.d0,50.d0,100.d0,200.d0,400.d0,1000.d0,10000.d0/        !/*ni         85*/
      data seeg /0.1d0,16.d0,35.d0,42.d0,360.d0,370.d0,310.d0,290.d0,430        !/*ni         86*/
     & .d0,435.d0,380.d0,350.d0,670.d0,650.d0,630.d0,610.d0,1130.d0,1040        !/*ni         87*/
     & .d0,1000.d0,1000.d0,1240.d0,1140.d0,1100.d0,1090.d0,1880.d0,1750         !/*ni         88*/
     & .d0,1645.d0,1630.d0,2930.d0,2750.d0,2540.d0,2500.d0,3240.d0,3050         !/*ni         89*/
     & .d0,2800.d0,2750.d0/                                                     !/*ni         90*/
      data pee /0.13d0,0.19d0,0.25d0,0.30d0/                                    !/*ni         91*/
!     data sig1/                                                                !/*ni         92*/
      data ((sig(i,j),i=1,20),j=1,5) /3*0.0001d0,0.1d0,1.d0,4.d0,24.5d0,        !/*ni         93*/
     & 25.d0,27.2d0,27.8d0,28.5d0,29.2d0,29.7d0,30.5d0,31.5d0,31.7d0,32.        !/*ni         94*/
     & 1d0,32.9d0,34.5d0,41.2d0,202.d0,194.d0,187.d0,175.d0,158.d0,153          !/*ni         95*/
     & .d0,217.d0,220.d0,212.d0,207.d0,203.d0,201.d0,199.d0,196.d0,194          !/*ni         96*/
     & .d0,195.d0,196.d0,199.d0,204.d0,226.d0,250.d0,240.d0,230.d0,216          !/*ni         97*/
     & .d0,196.d0,190.d0,250.d0,260.d0,259.d0,254.d0,249.d0,247.d0,244          !/*ni         98*/
     & .d0,241.d0,239.d0,240.d0,241.d0,244.d0,251.d0,276.d0,456.d0,434          !/*ni         99*/
     & .d0,416.d0,390.d0,361.d0,352.d0,467.d0,471.d0,458.d0,453.d0,450          !/*ni        100*/
     & .d0,455.d0,460.d0,445.d0,435.d0,430.d0,432.d0,437.d0,446.d0,481          !/*ni        101*/
     & .d0,782.d0,738.d0,706.d0,661.d0,626.d0,611.d0,776.d0,780.d0,764          !/*ni        102*/
     & .d0,760.d0,750.d0,760.d0,755.d0,740.d0,730.d0,726.d0,729.d0,736          !/*ni        103*/
     & .d0,748.d0,792.d0/                                                       !/*ni        104*/
!     data sig2/                                                                !/*ni        105*/
      data ((sig(i,j),i=1,20),j=6,9) /860.d0,810.d0,775.d0,725.d0,690.d0        !/*ni        106*/
     & ,680.d0,847.d0,853.d0,850.d0,845.d0,840.d0,835.d0,825.d0,805.d0,7        !/*ni        107*/
     & 95.d0,796.d0,799.d0,806.d0,819.d0,864.d0,1236.d0,1178.d0,1140.d0,        !/*ni        108*/
     & 1080.d0,1025.d0,1037.d0,1261.d0,1267.d0,1250.d0,1250.d0,1224.d0,1        !/*ni        109*/
     & 220.d0,1213.d0,1210.d0,1200.d0,1200.d0,1210.d0,1213.d0,1230.d0,12        !/*ni        110*/
     & 80.d0,1820.d0,1760.d0,1720.d0,1650.d0,1570.d0,1649.d0,1930.d0,193        !/*ni        111*/
     & 5.d0,1920.d0,1900.d0,1893.d0,1888.d0,1880.d0,1870.d0,1860.d0,1865        !/*ni        112*/
     & .d0,1870.d0,1880.d0,1900.d0,1945.d0,1990.d0,1930.d0,1890.d0,1820         !/*ni        113*/
     & .d0,1730.d0,1832.d0,2127.d0,2131.d0,2120.d0,2133.d0,2090.d0,2085         !/*ni        114*/
     & .d0,2080.d0,2070.d0,2060.d0,2060.d0,2070.d0,2077.d0,2095.d0,2140         !/*ni        115*/
     & .d0/                                                                     !/*ni        116*/
!     data seg1/                                                                !/*ni        117*/
      data ((seg(i,j),i=1,20),j=1,5) /42.d0,19.d0,16.1d0,17.d0,22.7d0,32        !/*ni        118*/
     & .5d0,24.6d0,26.2d0,25.0d0,23.7d0,23.0d0,22.5d0,22.d0,21.2d0,20.8d        !/*ni        119*/
     & 0,20.7d0,21.d0,21.9d0,23.8d0,28.4d0,290.d0,264.d0,212.d0,190.d0,1        !/*ni        120*/
     & 89.d0,197.d0,190.d0,185.d0,180.d0,175.d0,170.d0,168.d0,164.d0,155        !/*ni        121*/
     & .d0,145.d0,145.d0,148.d0,150.d0,155.d0,172.d0,350.d0,300.d0,250          !/*ni        122*/
     & .d0,240.d0,245.d0,260.d0,245.d0,230.d0,215.d0,210.d0,209.d0,208          !/*ni        123*/
     & .d0,205.d0,185.d0,175.d0,170.d0,170.d0,173.d0,181.d0,205.d0,610          !/*ni        124*/
     & .d0,540.d0,490.d0,460.d0,435.d0,450.d0,440.d0,415.d0,410.d0,398          !/*ni        125*/
     & .d0,392.d0,387.d0,370.d0,350.d0,333.d0,333.d0,335.d0,338.d0,348          !/*ni        126*/
     & .d0,383.d0,1000.d0,910.d0,860.d0,800.d0,780.d0,775.d0,760.d0,710         !/*ni        127*/
     & .d0,683.d0,670.d0,663.d0,654.d0,634.d0,598.d0,570.d0,570.d0,575          !/*ni        128*/
     & .d0,580.d0,600.d0,653.d0/                                                !/*ni        129*/
!     data seg2/                                                                !/*ni        130*/
      data ((seg(i,j),i=1,20),j=6,9) /1090.d0,1000.d0,960.d0,950.d0,880         !/*ni        131*/
     & .d0,850.d0,835.d0,780.d0,750.d0,740.d0,730.d0,720.d0,700.d0,660          !/*ni        132*/
     & .d0,630.d0,630.d0,635.d0,640.d0,660.d0,720.d0,1630.d0,1480.d0,131        !/*ni        133*/
     & 0.d0,1260.d0,1240.d0,1220.d0,1200.d0,1150.d0,1115.d0,1105.d0,1095        !/*ni        134*/
     & .d0,1080.d0,1062.d0,1000.d0,960.d0,960.d0,965.d0,971.d0,1000.d0,1        !/*ni        135*/
     & 085.d0,2500.d0,2250.d0,1960.d0,1850.d0,1820.d0,1800.d0,1780.d0,17        !/*ni        136*/
     & 00.d0,1690.d0,1685.d0,1680.d0,1675.d0,1660.d0,1580.d0,1500.d0,148        !/*ni        137*/
     & 0.d0,1480.d0,1495.d0,1530.d0,1650.d0,2750.d0,2475.d0,2200.d0,2100        !/*ni        138*/
     & .d0,2100.d0,2100.d0,2070.d0,1960.d0,1880.d0,1870.d0,1840.d0,1820         !/*ni        139*/
     & .d0,1800.d0,1740.d0,1665.d0,1665.d0,1678.d0,1692.d0,1744.d0,1878         !/*ni        140*/
     & .d0/                                                                     !/*ni        141*/
!     data segp1/                                                               !/*ni        142*/
      data ((segp(i,j),i=1,20),j=1,5) /0.1d0,0.1d0,0.1d0,0.1d0,11.d0,12         !/*ni        143*/
     & .d0,13.d0,14.d0,15.d0,16.d0,17.d0,18.d0,19.d0,19.7d0,19.9d0,20.d0        !/*ni        144*/
     & ,20.5d0,21.5d0,23.5d0,32.2d0,290.d0,264.d0,212.d0,190.d0,189.d0,1        !/*ni        145*/
     & 97.d0,190.d0,185.d0,180.d0,175.d0,170.d0,168.d0,164.d0,155.d0,145        !/*ni        146*/
     & .d0,145.d0,148.d0,150.d0,155.d0,172.d0,350.d0,300.d0,250.d0,240          !/*ni        147*/
     & .d0,245.d0,260.d0,245.d0,230.d0,215.d0,210.d0,209.d0,208.d0,205          !/*ni        148*/
     & .d0,185.d0,175.d0,170.d0,170.d0,173.d0,181.d0,205.d0,610.d0,540          !/*ni        149*/
     & .d0,490.d0,460.d0,435.d0,450.d0,440.d0,415.d0,410.d0,398.d0,392          !/*ni        150*/
     & .d0,387.d0,370.d0,350.d0,333.d0,333.d0,335.d0,338.d0,348.d0,383          !/*ni        151*/
     & .d0,1000.d0,910.d0,860.d0,800.d0,780.d0,775.d0,760.d0,710.d0,683         !/*ni        152*/
     & .d0,670.d0,663.d0,654.d0,634.d0,598.d0,570.d0,570.d0,575.d0,580          !/*ni        153*/
     & .d0,600.d0,653.d0/                                                       !/*ni        154*/
!     data segp2/                                                               !/*ni        155*/
      data ((segp(i,j),i=1,20),j=6,9) /1090.d0,1000.d0,960.d0,950.d0,880        !/*ni        156*/
     & .d0,850.d0,835.d0,780.d0,750.d0,740.d0,730.d0,720.d0,700.d0,660          !/*ni        157*/
     & .d0,630.d0,630.d0,635.d0,640.d0,660.d0,720.d0,1630.d0,1480.d0,131        !/*ni        158*/
     & 0.d0,1260.d0,1240.d0,1220.d0,1200.d0,1150.d0,1115.d0,1105.d0,1095        !/*ni        159*/
     & .d0,1080.d0,1062.d0,1000.d0,960.d0,960.d0,965.d0,971.d0,1000.d0,1        !/*ni        160*/
     & 085.d0,2500.d0,2250.d0,1960.d0,1850.d0,1820.d0,1800.d0,1780.d0,17        !/*ni        161*/
     & 00.d0,1690.d0,1685.d0,1680.d0,1675.d0,1660.d0,1580.d0,1500.d0,148        !/*ni        162*/
     & 0.d0,1480.d0,1495.d0,1530.d0,1650.d0,2750.d0,2475.d0,2200.d0,2100        !/*ni        163*/
     & .d0,2100.d0,2100.d0,2070.d0,1960.d0,1880.d0,1870.d0,1840.d0,1820         !/*ni        164*/
     & .d0,1800.d0,1740.d0,1665.d0,1665.d0,1678.d0,1692.d0,1744.d0,1878         !/*ni        165*/
     & .d0/                                                                     !/*ni        166*/
      data sigap /164.d0,126.d0,114.d0,98.d0,86.d0,72.4d0,59.d0,57.d0,53        !/*ni        167*/
     & .d0,52.d0,48.d0,45.5d0,43.5d0,40.4d0,36.5d0,35.2d0,34.5d0,34.5d0,        !/*ni        168*/
     & 35.4d0,41.5d0,7*324.d0,324.d0,301.d0,292.d0,284.d0,276.d0,272.d0,        !/*ni        169*/
     & 245.d0,202.d0,198.d0,196.d0,196.d0,199.d0,219.d0,7*388.d0,388.d0,        !/*ni        170*/
     & 360.d0,350.d0,340.d0,330.d0,325.d0,296.d0,246.d0,242.d0,240.d0,24        !/*ni        171*/
     & 0.d0,244.d0,266.d0,7*648.d0,648.d0,598.d0,581.d0,564.d0,548.d0,54        !/*ni        172*/
     & 0.d0,500.d0,434.d0,429.d0,427.d0,427.d0,432.d0,464.d0,7*1024.d0,1        !/*ni        173*/
     & 024.d0,941.d0,915.d0,888.d0,862.d0,849.d0,801.d0,720.d0,716.d0,71        !/*ni        174*/
     & 5.d0,715.d0,722.d0,763.d0,7*1110.d0,1110.d0,1020.d0,992.d0,963.d0        !/*ni        175*/
     & ,935.d0,921.d0,870.d0,788.d0,784.d0,784.d0,784.d0,792.d0,834.d0,7        !/*ni        176*/
     & *1590.d0,1590.d0,1457.d0,1417.d0,1376.d0,1336.d0,1316.d0,1260.d0,        !/*ni        177*/
     & 1175.d0,1172.d0,1176.d0,1176.d0,1185.d0,1233.d0,7*2338.d0,2338.d0        !/*ni        178*/
     & ,2136.d0,2076.d0,2017.d0,1958.d0,1928.d0,1873.d0,1800.d0,1804.d0,        !/*ni        179*/
     & 1816.d0,1816.d0,1826.d0,1874.d0,7*2552.d0,2552.d0,2330.d0,2265.d0        !/*ni        180*/
     & ,2200.d0,2136.d0,2103.d0,2050.d0,1984.d0,1990.d0,2004.d0,2004.d0,        !/*ni        181*/
     & 2015.d0,2061.d0/                                                         !/*ni        182*/
      data sigkm /38.d0,43.d0,23.d0,18.5d0,20.d0,29.d0,25.d0,23.d0,22.5d        !/*ni        183*/
     & 0,21.d0,20.5d0,20.d0,19.2d0,18.5d0,17.8d0,17.8d0,18.3d0,19.2d0,21        !/*ni        184*/
     & .2d0,28.9d0,7*174.4d0,174.4d0,161.6d0,149.d0,148.d0,150.d0,146.d0        !/*ni        185*/
     & ,131.d0,129.d0,129.d0,131.d0,136.d0,144.d0,174.d0,7*216.3d0,216.3        !/*ni        186*/
     & d0,200.4d0,187.d0,185.4d0,187.d0,182.d0,165.d0,161.d0,161.d0,164         !/*ni        187*/
     & .d0,169.d0,179.d0,219.d0,7*396.8d0,396.8d0,367.6d0,353.d0,350.d0,        !/*ni        188*/
     & 350.d0,341.d0,317.d0,303.d0,299.d0,302.d0,311.d0,328.d0,386.d0,7*        !/*ni        189*/
     & 683.8d0,683.8d0,633.6d0,624.d0,620.d0,616.d0,598.d0,569.d0,534.d0        !/*ni        190*/
     & ,522.d0,526.d0,539.d0,567.d0,654.d0,7*752.2d0,752.2d0,697.d0,690         !/*ni        191*/
     & .d0,685.d0,680.d0,660.d0,630.d0,590.d0,575.d0,579.d0,594.d0,624          !/*ni        192*/
     & .d0,718.d0,7*1153.d0,1153.d0,1068.3d0,1059.d0,1053.d0,1046.d0,101        !/*ni        193*/
     & 8.d0,937.d0,898.d0,886.d0,893.d0,915.d0,956.d0,1085.d0,7*1821.9d0        !/*ni        194*/
     & ,1821.9d0,1688.1d0,1675.d0,1670.d0,1660.d0,1620.d0,1433.d0,1407          !/*ni        195*/
     & .d0,1407.d0,1421.d0,1452.d0,1513.d0,1689.d0,7*2021.5d0,2021.5d0,1        !/*ni        196*/
     & 873.1d0,1859.d0,1854.d0,1844.d0,1800.d0,1620.d0,1580.d0,1563.d0,1        !/*ni        197*/
     & 580.d0,1613.d0,1679.d0,1868.d0/                                          !/*ni        198*/
      data sigkp /0.001d0,0.001d0,0.001d0,0.001d0,0.2d0,4.5d0,8.9d0,11.6        !/*ni        199*/
     & d0,12.2d0,13.4d0,13.6d0,13.7d0,13.7d0,14.9d0,15.9d0,16.5d0,17.4d0        !/*ni        200*/
     & ,18.6d0,20.9d0,28.8d0,7*174.4d0,129.6d0,124.6d0,126.1d0,124.6d0,1        !/*ni        201*/
     & 22.9d0,119.d0,117.d0,119.d0,129.d0,131.d0,136.d0,144.d0,174.d0,7*        !/*ni        202*/
     & 216.3d0,163.3d0,157.d0,158.8d0,157.d0,154.8d0,150.d0,148.d0,150          !/*ni        203*/
     & .d0,161.d0,164.d0,169.d0,179.d0,219.d0,7*317.5d0,313.2d0,301.1d0,        !/*ni        204*/
     & 304.6d0,301.1d0,296.8d0,287.d0,284.d0,286.d0,299.d0,302.d0,311.d0        !/*ni        205*/
     & ,328.d0,386.d0,7*683.8d0,561.8d0,540.2d0,546.4d0,540.2d0,532.5d0,        !/*ni        206*/
     & 515.d0,510.d0,510.d0,522.d0,526.d0,539.d0,567.d0,654.d0,7*752.2d0        !/*ni        207*/
     & ,622.4d0,598.4d0,605.3d0,598.4d0,589.9d0,570.d0,565.d0,565.d0,575        !/*ni        208*/
     & .d0,579.d0,594.d0,624.d0,718.d0,7*1153.d0,984.5d0,946.6d0,957.4d0        !/*ni        209*/
     & ,946.6d0,933.1d0,900.d0,886.d0,880.d0,886.d0,893.d0,915.d0,956.d0        !/*ni        210*/
     & ,1085.d0,7*1821.9d0,1608.8d0,1546.9d0,1564.6d0,1546.9d0,1524.8d0,        !/*ni        211*/
     & 1466.d0,1433.d0,1407.d0,1407.d0,1421.d0,1452.d0,1513.d0,1689.d0,7        !/*ni        212*/
     & *2021.5d0,1798.7d0,1729.6d0,1749.3d0,1729.6d0,1704.9d0,1640.d0,16        !/*ni        213*/
     & 00.d0,1570.d0,1563.d0,1580.d0,1613.d0,1679.d0,1868.d0/                   !/*ni        214*/
      dimension totcrs(19,5)                                                    !/*ni        215*/
      dimension plab(19)                                                        !/*ni        216*/
!  plab - lab momentum scale for totcrs                                         !/*ni        217*/
      data plab /.3d0,.4d0,.5d0,.6d0,.7d0,.8d0,.9d0,1.d0,1.1d0,1.2d0,1.3        !/*ni        218*/
     & d0,1.4d0,1.5d0,2.d0,3.d0,4.d0,5.d0,6.d0,10.d0/                           !/*ni        219*/
      data totcrs /79.4d0,76.2d0,44.7d0,36.5d0,33.1d0,40.2d0,43.4d0,51.7        !/*ni        220*/
     & d0,43.6d0,36.9d0,31.2d0,31.6d0,33.5d0,30.4d0,27.4d0,25.4d0,24.5d0        !/*ni        221*/
     & ,24.d0,22.5d0,54.2d0,51.d0,36.d0,26.d0,29.1d0,30.d0,30.d0,36.6d0,        !/*ni        222*/
     & 33.d0,29.6d0,28.3d0,27.d0,26.4d0,22.7d0,21.8d0,20.5d0,21.d0,21.9d        !/*ni        223*/
     & 0,20.6d0,12.d0,13.7d0,13.d0,12.5d0,11.2d0,12.d0,14.3d0,16.d0,17.2        !/*ni        224*/
     & d0,18.1d0,17.9d0,18.3d0,17.9d0,17.6d0,17.2d0,17.6d0,17.2d0,17.d0,        !/*ni        225*/
     & 17.3d0,13.d0,14.5d0,14.d0,13.d0,14.5d0,15.8d0,16.9d0,18.5d0,20.6d        !/*ni        226*/
     & 0,20.9d0,20.d0,19.4d0,19.d0,18.8d0,17.8d0,17.8d0,17.8d0,17.5d0,17        !/*ni        227*/
     & .5d0,280.d0,199.7d0,171.1d0,154.3d0,140.d0,130.d0,116.8d0,117.4d0        !/*ni        228*/
     & ,111.6d0,109.d0,106.5d0,102.8d0,100.d0,90.2d0,76.7d0,68.d0,62.8d0        !/*ni        229*/
     & ,60.7d0,56.d0/                                                           !/*ni        230*/
      data itt /1,7,0,0,0,0,0,2,8,0,0,10,3,4,6,5,1,2,10,1,1,1,3,10,9/           !/*ni        231*/
!                                                                               !/*ni        232*/
!     modified by a. ferrari to use rndm2                                       !/*ni        233*/
!                                                                               !/*ni        234*/
!     comment the next 1 card on vax: de-comment on ibm                         !/*ni        235*/
!                                                                               !/*ni        236*/
!     data rou1/-2.d0/                                                          !/*ni        237*/
!                                                                               !/*ni        238*/
!     comment the next 1 card on ibm: de-comment on vax                         !/*ni        239*/
!                                                                               !/*ni        240*/
      data irou1 /0/                                                            !/*ni        241*/
      aa=aaa                                                                    !/*ni        242*/
      si=1.0d-20                                                                !/*ni        243*/
      zl=1.0d+20                                                                !/*ni        244*/
      if (aa.lt.0.99d0) return                                                  !/*ni        245*/
!                                                                               !/*ni        246*/
!     calculate the new particle number iit:   1=p,2=n,3=pi+,4=pi-,             !/*ni        247*/
!     5=k-,6=k+,7=p bar,8=n bar,9=k zero bar,10=k zero                          !/*ni        248*/
!                                                                               !/*ni        249*/
      if (it.eq.30) return                                                      !/*ni        250*/
      iit=itt(it)                                                               !/*ni        251*/
      if (it.gt.25) iit=1                                                       !/*ni        252*/
      if (iit.eq.0) return                                                      !/*ni        253*/
!                                                                               !/*ni        254*/
!     rang is called only once even if 'call nizl' is in a do-loop              !/*ni        255*/
!     (i.e. current material is a compound).                                    !/*ni        256*/
!                                                                               !/*ni        257*/
      if (it.eq.19.or.it.eq.12) then                                            !/*ni        258*/
!                                                                               !/*ni        259*/
!       modified by a. ferrari (28-5-89) to use rndm2 instead of                !/*ni        260*/
!       rang                                                                    !/*ni        261*/
!                                                                               !/*ni        262*/
!     comment the next 4 cards on vax: de-comment on ibm                        !/*ni        263*/
!                                                                               !/*ni        264*/
!       call rdmout(rou2)                                                       !/*ni        265*/
!       if(rou1.ne.rou2) rnd=rang()                                             !/*ni        266*/
!       if(rnd.gt.0.5d0) iit=9                                                  !/*ni        267*/
!       call rdmout(rou1)                                                       !/*ni        268*/
!                                                                               !/*ni        269*/
!     comment the next 4 cards on ibm: de-comment on vax                        !/*ni        270*/
!                                                                               !/*ni        271*/
!       call rd2out(irou2,idum)                                                 !/*ni        272*/
!       if(irou1.ne.irou2) rnd=rang()                                           !/*ni        273*/
!       if(rnd.gt.0.5d0) iit=9                                                  !/*ni        274*/
!       call rd2out(irou1,idum)                                                 !/*ni        275*/
        iit=9                                                                   !/*ni        276*/
        if (rang().gt.0.5) iit=10                                               !/*ni        277*/
!     a.g. replacement to select k0 or k0-bar from kl and ks                    !/*ni        278*/
      endif                                                                     !/*ni        279*/
      if (aa.lt.2.d0) go to 10                                                  !/*ni        280*/
      if (iit.ge.5.and.po.le.2.d0) go to 150                                    !/*ni        281*/
!                                                                               !/*ni        282*/
!********************************************************************           !/*ni        283*/
!     p, n , pi+, pi-  or any high energy (> 2 gev/c) hadron                    !/*ni        284*/
!********************************************************************           !/*ni        285*/
!                                                                               !/*ni        286*/
!     calculate the momentum index k                                            !/*ni        287*/
!                                                                               !/*ni        288*/
   10 continue                                                                  !/*ni        289*/
      do 20 k=1,20                                                              !/*ni        290*/
      if (po.le.p(k)) go to 30                                                  !/*ni        291*/
   20 continue                                                                  !/*ni        292*/
      k=20                                                                      !/*ni        293*/
   30 continue                                                                  !/*ni        294*/
!                                                                               !/*ni        295*/
!     calculate the mass index j                                                !/*ni        296*/
!                                                                               !/*ni        297*/
      if (aa.ge.2.d0) go to 40                                                  !/*ni        298*/
      aa=1.                                                                     !/*ni        299*/
      j=1                                                                       !/*ni        300*/
      jj=1                                                                      !/*ni        301*/
      go to 70                                                                  !/*ni        302*/
   40 continue                                                                  !/*ni        303*/
      do 60 i=3,8                                                               !/*ni        304*/
      if (aa.le.a(i)) go to 50                                                  !/*ni        305*/
      go to 60                                                                  !/*ni        306*/
   50 continue                                                                  !/*ni        307*/
      j=i-1                                                                     !/*ni        308*/
      jj=j+1                                                                    !/*ni        309*/
      go to 70                                                                  !/*ni        310*/
   60 continue                                                                  !/*ni        311*/
      j=8                                                                       !/*ni        312*/
      jj=j+1                                                                    !/*ni        313*/
   70 continue                                                                  !/*ni        314*/
      go to (80,80,100,90,110,120,130,130,110,120), iit                         !/*ni        315*/
!                                                                               !/*ni        316*/
!     nucleons                                                                  !/*ni        317*/
!                                                                               !/*ni        318*/
   80 si1=sig(k,j)*(aa/a(j))**(log(sig(k,jj)/sig(k,j))/log(a(j+1)/a(j)))        !/*ni        319*/
#ifdef OLDM                                                                     /*!ni        320*/
      if (iit.eq.2) si1=si1/1.07d0                                              !/*ni        321*/
#endif                                                                          /*!ni        322*/
      si=si1                                                                    !/*ni        323*/
      if (k.eq.1.or.po.ge.10000.d0) go to 230                                   !/*ni        324*/
      kk=k-1                                                                    !/*ni        325*/
      si2 = sig(kk,j)*(aa/a(j))**(log(sig(kk,jj)/sig(kk,j))/                    !/*ni        326*/
     & log(a(j+1)/a(j)))                                                        !/*ni        327*/
#ifdef OLDM                                                                     /*!ni        328*/
      if (iit.eq.2) si2=si2/1.07                                                !/*ni        329*/
#endif                                                                          /*!ni        330*/
      go to 140                                                                 !/*ni        331*/
!                                                                               !/*ni        332*/
!     pi -                                                                      !/*ni        333*/
!                                                                               !/*ni        334*/
   90 continue                                                                  !/*ni        335*/
      if (k.eq.1) go to 200                                                     !/*ni        336*/
      si1=seg(k,j)*(aa/a(j))**(log(seg(k,jj)/seg(k,j))/log(a(j+1)/a(j)))        !/*ni        337*/
      si=si1                                                                    !/*ni        338*/
      if (k.eq.1.or.po.ge.10000.d0) go to 230                                   !/*ni        339*/
      kk=k-1                                                                    !/*ni        340*/
      si2=seg(kk,j)*(aa/a(j))**(log(seg(kk,jj)/seg(kk,j))/log(a(j+1)/a(j        !/*ni        341*/
     & )))                                                                      !/*ni        342*/
      go to 140                                                                 !/*ni        343*/
!                                                                               !/*ni        344*/
!     pi +                                                                      !/*ni        345*/
!                                                                               !/*ni        346*/
  100 continue                                                                  !/*ni        347*/
      if (k.eq.1) go to 200                                                     !/*ni        348*/
      si1=segp(k,j)*(aa/a(j))**(log(segp(k,jj)/segp(k,j))/log(a(j+1)/a(j        !/*ni        349*/
     & )))                                                                      !/*ni        350*/
      si=si1                                                                    !/*ni        351*/
      if (k.eq.1.or.po.ge.10000.d0) go to 230                                   !/*ni        352*/
      kk=k-1                                                                    !/*ni        353*/
      si2=segp(kk,j)*(aa/a(j))**(log(segp(kk,jj)/segp(kk,j))/log(a(j+1)         !/*ni        354*/
     & /a(j)))                                                                  !/*ni        355*/
      go to 140                                                                 !/*ni        356*/
!                                                                               !/*ni        357*/
!     k -  and k0 bar                                                           !/*ni        358*/
!                                                                               !/*ni        359*/
  110 continue                                                                  !/*ni        360*/
!     if(k.eq.1) goto 1113                                                      !/*ni        361*/
      si1=sigkm(k,j)*(aa/a(j))**(log(sigkm(k,jj)/sigkm(k,j))/log(a(j+1)         !/*ni        362*/
     & /a(j)))                                                                  !/*ni        363*/
      si=si1                                                                    !/*ni        364*/
      if (k.eq.1.or.po.ge.10000.d0) go to 230                                   !/*ni        365*/
      kk=k-1                                                                    !/*ni        366*/
      si2=sigkm(kk,j)*(aa/a(j))**(log(sigkm(kk,jj)/sigkm(kk,j))/log(a(j+        !/*ni        367*/
     & 1)/a(j)))                                                                !/*ni        368*/
      go to 140                                                                 !/*ni        369*/
!                                                                               !/*ni        370*/
!     k +  and k0                                                               !/*ni        371*/
!                                                                               !/*ni        372*/
  120 continue                                                                  !/*ni        373*/
      si1=sigkp(k,j)*(aa/a(j))**(log(sigkp(k,jj)/sigkp(k,j))/log(a(j+1)         !/*ni        374*/
     & /a(j)))                                                                  !/*ni        375*/
      si=si1                                                                    !/*ni        376*/
      if (k.eq.1.or.po.ge.10000.d0) go to 230                                   !/*ni        377*/
      kk=k-1                                                                    !/*ni        378*/
      si2=sigkp(kk,j)*(aa/a(j))**(log(sigkp(kk,jj)/sigkp(kk,j))/log(a(j+        !/*ni        379*/
     & 1)/a(j)))                                                                !/*ni        380*/
      go to 140                                                                 !/*ni        381*/
!                                                                               !/*ni        382*/
!     anti-nucleons                                                             !/*ni        383*/
!                                                                               !/*ni        384*/
  130 continue                                                                  !/*ni        385*/
      si1=sigap(k,j)*(aa/a(j))**(log(sigap(k,jj)/sigap(k,j))/log(a(j+1)         !/*ni        386*/
     & /a(j)))                                                                  !/*ni        387*/
      si=si1                                                                    !/*ni        388*/
      if (k.eq.1.or.po.ge.10000.d0) go to 230                                   !/*ni        389*/
      kk=k-1                                                                    !/*ni        390*/
      si2=sigap(kk,j)*(aa/a(j))**(log(sigap(kk,jj)/sigap(kk,j))/log(a(j+        !/*ni        391*/
     & 1)/a(j)))                                                                !/*ni        392*/
!                                                                               !/*ni        393*/
!     interpolate linearly with respect to momentum                             !/*ni        394*/
!                                                                               !/*ni        395*/
  140 si=si1+(po-p(k))*(si2-si1)/(p(kk)-p(k))                                   !/*ni        396*/
      go to 230                                                                 !/*ni        397*/
!                                                                               !/*ni        398*/
!********************************************************************           !/*ni        399*/
!     low energy  (<2.0 gev/c) k-, k+, pbar, nbar, k zero bar, k zero           !/*ni        400*/
!********************************************************************           !/*ni        401*/
!                                                                               !/*ni        402*/
  150 continue                                                                  !/*ni        403*/
      if (iit.ge.9) iit=iit-4                                                   !/*ni        404*/
!                                                                               !/*ni        405*/
!     calculate momentum index k and interaction indices i1 and i2              !/*ni        406*/
!                                                                               !/*ni        407*/
      do 160 k=1,19                                                             !/*ni        408*/
      if (po.le.plab(k)) go to 170                                              !/*ni        409*/
  160 continue                                                                  !/*ni        410*/
      k=19                                                                      !/*ni        411*/
  170 kk=k-1                                                                    !/*ni        412*/
      po1=po-plab(k)                                                            !/*ni        413*/
      iit=iit-4                                                                 !/*ni        414*/
      i2=2*iit                                                                  !/*ni        415*/
      i1=i2-1                                                                   !/*ni        416*/
      if (i1.lt.5) go to 180                                                    !/*ni        417*/
      i1=5                                                                      !/*ni        418*/
      i2=5                                                                      !/*ni        419*/
  180 continue                                                                  !/*ni        420*/
!                                                                               !/*ni        421*/
!     take the average over -/neutron and -/proton cross sections               !/*ni        422*/
!     and interpolate linearly with respect to momentum                         !/*ni        423*/
!                                                                               !/*ni        424*/
      si=(totcrs(k,i1)+totcrs(k,i2))*0.5d0                                      !/*ni        425*/
      if (k.eq.1) go to 190                                                     !/*ni        426*/
      ds=(totcrs(kk,i1)+totcrs(kk,i2)-totcrs(k,i1)-totcrs(k,i2))*0.5d0          !/*ni        427*/
      si=si+po1*ds/(plab(kk)-plab(k))                                           !/*ni        428*/
  190 si=bet(iit)*si*aa**alph(iit)                                              !/*ni        429*/
      if (it.ne.16.or.po.ge.1.41d0) go to 230                                   !/*ni        430*/
!                                                                               !/*ni        431*/
!     special treatment for low energy k-                                       !/*ni        432*/
!                                                                               !/*ni        433*/
      si=si*0.5d0*(1.d0+sqrt(po**2+0.244d0)-0.494d0)                            !/*ni        434*/
      go to 230                                                                 !/*ni        435*/
!                                                                               !/*ni        436*/
!********************************************************************           !/*ni        437*/
!        low energy pions (<0.3gev/c)                                           !/*ni        438*/
!********************************************************************           !/*ni        439*/
!                                                                               !/*ni        440*/
  200 continue                                                                  !/*ni        441*/
      si=0.01d0                                                                 !/*ni        442*/
      if (it.eq.13.and.j.eq.1) go to 230                                        !/*ni        443*/
      do 210 k=1,4                                                              !/*ni        444*/
      if (po.le.pee(k)) go to 220                                               !/*ni        445*/
  210 continue                                                                  !/*ni        446*/
      k=4                                                                       !/*ni        447*/
  220 continue                                                                  !/*ni        448*/
      si1=seeg(k,j)*(aa/a(j))**(log(seeg(k,jj)/seeg(k,j))/log(a(j+1)/a(j        !/*ni        449*/
     & )))                                                                      !/*ni        450*/
      si=si1                                                                    !/*ni        451*/
      if (k.eq.1) go to 230                                                     !/*ni        452*/
      kk=k-1                                                                    !/*ni        453*/
      si2=seeg(kk,j)*(aa/a(j))**(log(seeg(kk,jj)/seeg(kk,j))/log(a(j+1)         !/*ni        454*/
     & /a(j)))                                                                  !/*ni        455*/
!                                                                               !/*ni        456*/
!     interpolate linearly with respect to momentum                             !/*ni        457*/
!                                                                               !/*ni        458*/
      si=si1+(po-pee(k))*(si2-si1)/(pee(kk)-pee(k))                             !/*ni        459*/
!                                                                               !/*ni        460*/
!********************************************************************           !/*ni        461*/
!     calculate the interaction length                                          !/*ni        462*/
!********************************************************************           !/*ni        463*/
!                                                                               !/*ni        464*/
  230 continue                                                                  !/*ni        465*/
#ifdef OLDM                                                                     /*!ni        466*/
      if (j.eq.1.and.iit.eq.2) si=si*1.07d0                                     !/*ni        467*/
#endif                                                                          /*!ni        468*/
      zl=10000.d0*aa/(6.022d0*si)                                               !/*ni        469*/
      return                                                                    !/*ni        470*/
      end                                                                       !/*ni        471*/
