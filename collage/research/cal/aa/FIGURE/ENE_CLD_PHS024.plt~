#!/usr/bin/gnuplot -persist
set term postscript enh color solid 10 
set output "ENE_CLD_PHS024.ps"
set xrange[0:2]
unset xtics
#set format x""
set multiplot

#######################################################################
# L=0
#######################################################################
set yrange[-45:200]
set ytics -45,45,180
set noy2tics
set ylabel "{/Symbol d} [deg.]" font "Times,20" 
set label "{/Symbol d}" at 1,120 center font "Times,20"
set label "L=0" at 1,215 center font "Times,20"
#E_{r}=0.094MeV {/Symbol G}=1.16*10^{-3}MeV
set origin 0,0.7
set size 0.33,0.3
set tmargin 0
set bmargin 0
set rmargin 0
p "../PHS/DATA/ANA/AAPHS0.d" u 1:2 t "" w l lt 3 lw 3.0,\
  0 t "" lw 3 lt 0

#######################################################################
set yrange[-0.5:1.5]
set ytics -0.5,0.5,1
set noy2tics
set nox2label
unset label
set label "{/Symbol D}" at 1,0.5 center font "Times,20"
set ylabel "{/Symbol D}(E) [MeV^{-1}]" font "Times,20"
set origin 0,0.4
set size 0.33,0.3
set tmargin 0
set rmargin 0
p "../CLD/DATA/AACLD0.d" u 1:2 t "" w l lt 3 lw 3.0 
#######################################################################
set yrange[-2:0]
set xtics 0,0.5,1.5
set ytics -2,0.5,-0.5
set noy2tics
set xlabel "Re-Energy [MeV]" font "Times,20"
set nox2label
set ylabel "Im-Energy [MeV]" font "Times,20"
unset label
set label "E" at 1,-1 center font "Times,20"
set origin 0.004,0.1
set size 0.326,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 8.5
p "../ENE/DATA/AAENE0.d" u 2:3 t "" w p lt 3 pt 7 ps 1.0
#######################################################################
# L=2
#######################################################################
set xrange[0:20]
set yrange[-45:200]
unset xtics
unset ytics
#set ytics (0,45,90,135,180)
unset xlabel
unset ylabel
#set ylabel "{/Symbol d} [deg.]"
unset label
set label "L=2" at 10,215 center font "Times,20"
#E_{r}=2.78MeV {/Symbol G}=1.32MeV
set origin 0.33,0.7
set size 0.27,0.3
set bmargin 0
set lmargin 0
p "../PHS/DATA/ANA/AAPHS2.d" u 1:2 t "" w l lt 3 lw 3.0,\
  0 t "" lw 3 lt 0

#######################################################################
set yrange[-0.5:1.5]
unset xtics
unset ytics
#set ytics (-0.5,0,0.5,1)
unset xlabel
set nox2label
#set ylabel "{/Symbol D}(E) [MeV^{-1}]"
unset ylabel
unset label
set origin 0.33,0.4
set size 0.27,0.3
set lmargin 0
p "../CLD/DATA/AACLD2.d" u 1:2 t "" w l lt 3 lw 3.0

#######################################################################
set yrange[-6:0]
set xtics 0,5,18
#set ytics (-8,-6,-4,-2)
unset ytics
set xlabel "Re-Energy [MeV]" font "Times,20"
set nox2label
#set ylabel "Im-Energy [MeV]"
unset ylabel
set origin 0.33,0.1
set size 0.27,0.3
set bmargin 0
set lmargin 0
#set arrow from 2.08,-8 to 2.08,16 nohead lt 1 lw 1
p "../ENE/DATA/AAENE2.d" u 2:3 t "" w p lt 3 pt 7 ps 1.0
#######################################################################
# L=4
#######################################################################
set yrange[-45:200]
unset xtics
#set ytics (0,45,90,135,180)
unset xlabel
unset ylabel
unset label
set label "L=4" at 10,215 center font "Times,20"
#E_{r}=11.7MeV {/Symbol G}=4.46MeV
set origin 0.6,0.7
set size 0.27,0.3
set bmargin 0
p "../PHS/DATA/ANA/AAPHS4.d" u 1:2 t "" w l lt 3 lw 3.0,\
  0 t "" lw 3 lt 0

#######################################################################
set yrange[-0.5:1.5]
unset xtics
unset xlabel 
unset x2label
set origin 0.6,0.4
set size 0.27,0.3
p "../CLD/DATA/AACLD4.d" u 1:2 t "" w l lt 3 lw 3.0

#######################################################################
set yrange[-6:0]
set xtics 0,5,20
#set ytics (-8,-6,-4,-2)
unset ytics
set xlabel "Re-Energy [MeV]" font "Times,20"
unset x2label
#set ylabel "Im-Energy [MeV]"
set origin 0.6,0.1
set size 0.27,0.3
set bmargin 0
#set arrow from 0.78,-8 to 0.78,16 nohead lt 1 lw 3
p "../ENE/DATA/AAENE4.d" u 2:3 t "" w p lt 3 pt 7 ps 1.0
unset multiplot
