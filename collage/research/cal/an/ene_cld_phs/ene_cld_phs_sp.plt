#!/usr/bin/gnuplot -persist
set term postscript enh color
set output "ene_cld_phs_sp.eps"
set xrange[0:10]
set multiplot

#######################################################################
# s_{1/2}
#######################################################################
set yrange[-30:180]
set ytics (0,45,90,135,180)
unset xtics
set key
set noy2tics
set ylabel "{/Symbol d} [deg.]"
set label "s_{1/2}" at 2.5,165 center font "Times,15"
set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.1,0.7
set size 0.3,0.29
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 3
p "phs_ana.d" u 1:($2+180) t "" w l lt -1 lw 1.0

#######################################################################
set yrange[-0.5:1.5]
set ytics (-0.5,0,0.5,1)
set noy2tics
set nox2label
unset label
set label "{/Symbol D}" at 4.5,0.25 center font "Times,20"
set ylabel "{/Symbol D}(E) [MeV^{-1}]"
set origin 0.1,0.4
set size 0.3,0.3
set tmargin 0
set rmargin 0
p "cld_s1_100MeV.d" u 1:2 t "" w l lt -1 lw 1.0 
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
set label "E" at 1.5,-4 center font "Times,20"
set origin 0.1,0.1
set size 0.3,0.3
set tmargin 0
set bmargin 0
set rmargin 0
#set arrow from 2.08,-8 to 2.08,16 nohead lt 1 lw 1
p "ene_s1.d" u 2:3 t "" w p lt 3 pt 7 ps 1.0

#######################################################################
# p_{3/2}
#######################################################################
set yrange[-30:180]
unset xtics
unset ytics
#set ytics (0,45,90,135,180)
unset xlabel
unset ylabel
#set ylabel "{/Symbol d} [deg.]"
unset label
set key 4,90
set label "p_{3/2}    E_{r}=0.78MeV {/Symbol G}=0.55MeV" at 2.5,165 center font "Times,15"
set origin 0.4,0.7
set size 0.3,0.29
set bmargin 0
set lmargin 0
p "../phs/allphs/allphs_p3_100MeV.d" u 1:2 t "total" w l lt -1 lw 1.0,\
  "../phs/allphs/allphs_p3_100MeV.d" u 1:3 t "resonance" w l lt 0 lw 5.0,\
  "../phs/allphs/allphs_p3_100MeV.d" u 1:4 t "continuum" w l lt 2 lw 3.0

#######################################################################
set yrange[-0.5:1.5]
unset xtics
#set ytics (0,0.5,1)
unset xlabel 
unset x2label
set key 4,0.7
#set ylabel "{/Symbol D}(E) [MeV^{-1}]"
set origin 0.4,0.4
set size 0.3,0.3
p "../cld/allcld/allcld_p3_100MeV.d" u 1:2 t "total" w l lt -1 lw 1.0,\
  "../cld/allcld/allcld_p3_100MeV.d" u 1:3 t "resonance" w l lt 0 lw 5.0,\
  "../cld/allcld/allcld_p3_100MeV.d" u 1:4 t "continuum" w l lt 2 lw 3.0 

#######################################################################
set xrange[0:5]
set yrange[-6:0]
set xtics 0,1,4
#set ytics (-8,-6,-4,-2)
unset ytics
set xlabel "Re-Energy [MeV]"
unset x2label
unset key
#set ylabel "Im-Energy [MeV]"
set origin 0.4,0.1
set size 0.3,0.3
set bmargin 0
#set arrow from 0.78,-8 to 0.78,16 nohead lt 1 lw 3
p "ene_p3.d" u 2:3 t "" w p lt 3 pt 7 ps 1.0

#######################################################################
# p_{1/2}
#######################################################################
set yrange[-30:180]
unset xtics
#set ytics (0,45,90,135,180)
unset xlabel
unset ylabel
#set ylabel "{/Symbol d} [deg.]"
unset label
set label "p_{1/2}   E_{r}=2.08MeV {/Symbol G}=5.67MeV" at 2.5,165 center font "Times,15"
set origin 0.7,0.7
set size 0.29,0.29
set bmargin 0
set lmargin 0
p "../phs/allphs/allphs_p1_100MeV.d" u 1:2 t "" w l lt -1 lw 1.0 ,\
  "../phs/allphs/allphs_p1_100MeV.d" u 1:3 t "" w l lt 0 lw 5.0 ,\
  "../phs/allphs/allphs_p1_100MeV.d" u 1:4 t "" w l lt 2 lw 3.0 

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
set origin 0.7,0.4
set size 0.29,0.3
set lmargin 0
p "../cld/allcld/allcld_p1_100MeV.d" u 1:2 t "" w l lt -1 lw 1.0,\
  "../cld/allcld/allcld_p1_100MeV.d" u 1:3 t "" w l lt 0 lw 5.0,\
  "../cld/allcld/allcld_p1_100MeV.d" u 1:4 t "" w l lt 2 lw 3.0 

#######################################################################
set xrange[0:5]
set yrange[-6:0]
set xtics 0,1,5
#set ytics (-8,-6,-4,-2)
unset ytics
set xlabel "Re-Energy [MeV]"
set nox2label
#set ylabel "Im-Energy [MeV]"
unset ylabel
set origin 0.7,0.1
set size 0.29,0.3
set bmargin 0
set lmargin 0
#set arrow from 2.08,-8 to 2.08,16 nohead lt 1 lw 1
p "ene_p1.d" u 2:3 t "" w p lt 3 pt 7 ps 1.0

unset multiplot
