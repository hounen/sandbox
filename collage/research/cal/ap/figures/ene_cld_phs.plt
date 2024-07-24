#!/usr/bin/gnuplot -persist
set term postscript enh color solid 8 portrait 
set output "ene_cld_phs.ps"

set xrange[0:5]
unset xtics

#set format x""
set multiplot

#######################################################################
# s_{1/2}
#######################################################################
set yrange[-30:180]
set ytics (0,45,90,135,180)
set noy2tics
set ylabel "{/Symbol d} [deg.]"
set label "s_{1/2}" at 2.5,160 center
set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0,0.2
set size 0.33,0.1
set tmargin 0
set bmargin 0
set rmargin 0
p "../phs/data/analysis/phs_ana_s1.d" u 1:($2+180) t "" w l lt 3 lw 3.0

#######################################################################
set yrange[-0.5:1.5]
set ytics (-0.5,0,0.5,1)
set noy2tics
set nox2label
unset label
set label "{/Symbol D}" at 4.5,0.25 center font "Times,15"
set ylabel "{/Symbol D}(E) [MeV^{-1}]"
set origin 0,0.1
set size 0.33,0.1
set tmargin 0
set rmargin 0
p "../cld/data/cld_s1.d" u 1:2 t "" w l lt 3 lw 3.0 
#######################################################################
set xrange[0:5]
set yrange[-6:0]
set xtics 0,1,4
set ytics (-8,-6,-4,-2)
set noy2tics
set xlabel "Re-Energy [MeV]"
set nox2label
set ylabel "Im-Energy [MeV]"
unset label
set label "E" at 1.5,-4 center font "Times,15"
set origin 0,0
set size 0.33,0.1
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 8.5
#set arrow from 2.08,-8 to 2.08,16 nohead lt 1 lw 1
p "../ene/data/ene_s1.d" u 2:3 t "" w p lt 3 pt 7 ps 1.0
#######################################################################
# p_{1/2}
#######################################################################
set yrange[-30:180]
unset xtics
unset ytics
#set ytics (0,45,90,135,180)
unset xlabel
unset ylabel
#set ylabel "{/Symbol d} [deg.]"
unset label
set label "p_{1/2}   E_{r}=2.08MeV {/Symbol G}=5.67MeV" at 2.5,160 center
set origin 0.33,0.2
set size 0.27,0.1
set bmargin 0
set lmargin 0
p "../phs/data/analysis/phs_ana_p1.d" u 1:2 t "" w l lt 3 lw 3.0,\
  "../phs/data/analysis/phs_ana_p1.d" u 1:3 t "" w l lt 1 lw 3.0,\
  "../phs/data/analysis/phs_ana_p1.d" u 1:4 t "" w l lt 2 lw 3.0

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
set origin 0.33,0.1
set size 0.27,0.1
set lmargin 0
p "../cld/data/cld_p1.d" u 1:2 t "" w l lt 3 lw 3.0,\
  "../cld/data/cld_p1.d" u 1:3 t "" w l lt 1 lw 3.0,\
  "../cld/data/cld_p1.d" u 1:4 t "" w l lt 2 lw 3.0 

#######################################################################
set xrange[0:5]
set yrange[-6:0]
set xtics 0,1,4
#set ytics (-8,-6,-4,-2)
unset ytics
set xlabel "Re-Energy [MeV]"
set nox2label
#set ylabel "Im-Energy [MeV]"
unset ylabel
set origin 0.33,0.0
set size 0.27,0.1
set bmargin 0
set lmargin 0
#set arrow from 2.08,-8 to 2.08,16 nohead lt 1 lw 1
p "../ene/data/ene_p1.d" u 2:3 t "" w p lt 3 pt 7 ps 1.0

#######################################################################
# p_{3/2}
#######################################################################
set yrange[-30:180]
unset xtics
#set ytics (0,45,90,135,180)
unset xlabel
unset ylabel
#set ylabel "{/Symbol d} [deg.]"
unset label
set label "p_{3/2}    E_{r}=0.78MeV {/Symbol G}=0.55MeV" at 2.5,160 center
set origin 0.6,0.2
set size 0.27,0.1
set bmargin 0
p "../phs/data/analysis/phs_ana_p3.d" u 1:2 t "" w l lt 3 lw 3.0,\
  "../phs/data/analysis/phs_ana_p3.d" u 1:3 t "" w l lt 1 lw 3.0,\
  "../phs/data/analysis/phs_ana_p3.d" u 1:4 t "" w l lt 2 lw 3.0

#######################################################################
set yrange[-0.3:1.5]
unset xtics
#set ytics (0,0.5,1)
unset xlabel 
unset x2label
#set ylabel "{/Symbol D}(E) [MeV^{-1}]"
set origin 0.6,0.1
set size 0.27,0.1
p "../cld/data/cld_p3.d" u 1:2 t "" w l lt 3 lw 3.0,\
  "../cld/data/cld_p3.d" u 1:3 t "" w l lt 1 lw 3.0,\
  "../cld/data/cld_p3.d" u 1:4 t "" w l lt 2 lw 3.0 

#######################################################################
set xrange[0:5]
set yrange[-6:0]
set xtics 0,1,5
#set ytics (-8,-6,-4,-2)
unset ytics
set xlabel "Re-Energy [MeV]"
unset x2label
#set ylabel "Im-Energy [MeV]"
set origin 0.6,0.0
set size 0.27,0.1
set bmargin 0
#set arrow from 0.78,-8 to 0.78,16 nohead lt 1 lw 3
p "../ene/data/ene_p3.d" u 2:3 t "" w p lt 3 pt 7 ps 1.0
#######################################################################
# d_{3/2}
#######################################################################
#set xrange[0:50]
#set yrange[-50:100]
#unset xtics
##set ytics (0,45,90,135,180)
#unset ytics
#unset xlabel
##set ylabel "{/Symbol d} [deg.]"
#unset ylabel
#unset label
#set label "d_{3/2}   E_{r}=29.3MeV {/Symbol G}=37.6MeV" at 25,90 center
#set origin 0.6,0.55
#set size 0.27,0.1
#set bmargin 0
#p "../phs/allphs/allphs_d3_100MeV.d" u 1:2 t "" w l lt 3 lw 3.0,\
#  "../phs/allphs/allphs_d3_100MeV.d" u 1:3 t "" w l lt 1 lw 3.0,\
#  "../phs/allphs/allphs_d3_100MeV.d" u 1:4 t "" w l lt 2 lw 3.0
#unset label
#
########################################################################
#set xrange[0:50]
#set yrange[-0.01:0.02]
#unset xtics
##set ytics (-0.5,0,0.5,1)
#unset ytics
#unset xlabel
#set nox2label
##set ylabel "{/Symbol D}(E) [MeV^{-1}]"
#unset ylabel
#set origin 0.6,0.45
#set size 0.27,0.1
#p "../cld/allcld/allcld_d3_100MeV.d" u 1:2 t "" w l lt 3 lw 3.0,\
#  "../cld/allcld/allcld_d3_100MeV.d" u 1:3 t "" w l lt 1 lw 3.0,\
#  "../cld/allcld/allcld_d3_100MeV.d" u 1:4 t "" w l lt 2 lw 3.0 
#
########################################################################
#set xrange[0:50]
#set yrange[-140:0]
#set xtics 0,10,50
##set ytics -140,20,0
#unset ytics
#set xlabel "Re-Energy [MeV]"
#set nox2label
##set ylabel "Im-Energy [MeV]"
#unset ylabel
#set origin 0.6,0.35
#set size 0.27,0.1
#set bmargin 0
#set rmargin 0
##set arrow from 2.08,-8 to 2.08,16 nohead lt 1 lw 1
#p "ene_d3.d" u 2:3 t "" w p lt 3 pt 7 ps 1.0
########################################################################
## d_{5/2}
########################################################################
#set xrange[0:50]
#set yrange[-50:100]
#unset xtics
#set ytics -10,30,80
#unset xlabel
#set ylabel "{/Symbol d} [deg.]"
#unset label
#set label "{/Symbol d}" at 45,50 center font "Times,15"
#set label "d_{5/2}    E_{r}=28.5MeV {/Symbol G}=43.1MeV" at 25,90 center
#set origin 0.33,0.55
#set size 0.27,0.1
#set bmargin 0
#p "../phs/allphs/allphs_d5_100MeV.d" u 1:2 t "" w l lt 3 lw 3.0,\
#  "../phs/allphs/allphs_d5_100MeV.d" u 1:3 t "" w l lt 1 lw 3.0,\
#  "../phs/allphs/allphs_d5_100MeV.d" u 1:4 t "" w l lt 2 lw 3.0
########################################################################
#set xrange[0:50]
#set yrange[-0.01:0.02]
#unset xtics
#set ytics -0.005,0.005,0.015
#unset xlabel 
#unset x2label
#set ylabel "{/Symbol D}(E) [MeV^{-1}]"
#set origin 0.33,0.45
#set size 0.27,0.1
#unset label
#set label "{/Symbol D}" at 45,0.0025 center font "Times,15"
#p "../cld/allcld/allcld_d5_100MeV.d" u 1:2 t "" w l lt 3 lw 3.0,\
#  "../cld/allcld/allcld_d5_100MeV.d" u 1:3 t "" w l lt 1 lw 3.0,\
#  "../cld/allcld/allcld_d5_100MeV.d" u 1:4 t "" w l lt 2 lw 3.0 
########################################################################
#set xrange[0:50]
#set yrange[-140:0]
#set xtics 0,10,40
#set ytics -140,35,-35
#set xlabel "Re-Energy [MeV]"
#unset x2label
#set ylabel "Im-Energy [MeV]"
#unset label
#set label "E" at 25,-70 center font "Times,15"
#set origin 0.33,0.35
#set size 0.27,0.1
#set bmargin 0
##set arrow from 0.78,-8 to 0.78,16 nohead lt 1 lw 3
#p "ene_d5.d" u 2:3 t "" w p lt 3 pt 7 ps 1.0
########################################################################
## f_{5/2}
########################################################################
#set yrange[-30:100]
#unset xtics
#set ytics 0,20,100
#unset xlabel
#unset label
#set label "f_{5/2}   E_{r}=17.8MeV {/Symbol G}=64.3MeV" at 25,90 center
#set ylabel "{/Symbol d} [deg.]"
#set label "{/Symbol d}" at 45,30 center font "Times,15"
#set origin 0.33,0.9
#set size 0.27,0.1
#set bmargin 0
#p "../phs/allphs/allphs_f5_100MeV.d" u 1:2 t "" w l lt 3 lw 3.0,\
#  "../phs/allphs/allphs_f5_100MeV.d" u 1:3 t "" w l lt 1 lw 3.0,\
#  "../phs/allphs/allphs_f5_100MeV.d" u 1:4 t "" w l lt 2 lw 3.0
#
########################################################################
#set yrange[-0.01:0.02]
#unset xtics
#set ytics -0.005,0.005,0.015
#unset label
#unset xlabel
#set nox2label
#set ylabel "{/Symbol D}(E) [MeV^{-1}]"
#set label "{/Symbol D}" at 45,0.003 center font "Times,15"
#set origin 0.33,0.8
#set size 0.27,0.1
#p "../cld/allcld/allcld_f5_100MeV.d" u 1:2 t "" w l lt 3 lw 3.0,\
#  "../cld/allcld/allcld_f5_100MeV.d" u 1:3 t "" w l lt 1 lw 3.0,\
#  "../cld/allcld/allcld_f5_100MeV.d" u 1:4 t "" w l lt 2 lw 3.0 
########################################################################
#set xrange[0:50]
#set yrange[-140:0]
#set xtics 0,10,40
#set ytics -140,35,-35
#set xlabel "Re-Energy [MeV]"
#set nox2label
#set ylabel "Im-Energy [MeV]"
#unset label
#set label "E" at 25,-70 center font "Times,15"
#set origin 0.33,0.7
#set size 0.27,0.1
#set bmargin 0
##set arrow from 2.08,-8 to 2.08,16 nohead lt 1 lw 1
#p "ene_f5.d" u 2:3 t "" w p lt 3 pt 7 ps 1.0
#unset label
########################################################################
## f_{7/2}
########################################################################
#set xrange[0:50]
#set yrange[-30:100]
#unset xtics
##set ytics (0,45,90,135,180)
#unset xlabel
#set label "f_{7/2}    E_{r}=25.4MeV {/Symbol G}=55.7MeV" at 25,90 center
#unset ylabel
##set ylabel "{/Symbol d} [deg.]"
#set origin 0.6,0.9
#set size 0.27,0.1
#set bmargin 0
#p "../phs/allphs/allphs_f7_100MeV.d" u 1:2 t "" w l lt 3 lw 3.0,\
#  "../phs/allphs/allphs_f7_100MeV.d" u 1:3 t "" w l lt 1 lw 3.0,\
#  "../phs/allphs/allphs_f7_100MeV.d" u 1:4 t "" w l lt 2 lw 3.0
#unset label
########################################################################
#set xrange[0:50]
#set yrange[-0.01:0.02]
#unset xtics
##set ytics (0,0.5,1)
#unset ytics
#unset xlabel 
#unset x2label
##set ylabel "{/Symbol D}(E) [MeV^{-1}]"
#set origin 0.6,0.8
#set size 0.27,0.1
#p "../cld/allcld/allcld_f7_100MeV.d" u 1:2 t "" w l lt 3 lw 3.0,\
#  "../cld/allcld/allcld_f7_100MeV.d" u 1:3 t "" w l lt 1 lw 3.0,\
#  "../cld/allcld/allcld_f7_100MeV.d" u 1:4 t "" w l lt 2 lw 3.0 
#
########################################################################
#set xrange[0:50]
#set yrange[-140:0]
#set xtics 0,10,50
##set ytics (-8,-6,-4,-2)
#set xlabel "Re-Energy [MeV]"
#unset x2label
#unset ytics
##set ylabel "Im-Energy [MeV]"
#set origin 0.6,0.7
#set size 0.27,0.1
#set bmargin 0
##set arrow from 0.78,-8 to 0.78,16 nohead lt 1 lw 3
#p "ene_f7.d" u 2:3 t "" w p lt 3 pt 7 ps 1.0
unset multiplot
