#!/usr/bin/gnuplot -persist
set term postscript eps enh color solid
set output "ene_cld_phs_p3.eps"

set xrange [0:5]
unset xtics

#set format x""
set multiplot

set x2label "p_{3/2}    E_{r}=0.78MeV {/Symbol G}=0.55"
set ylabel "{/Symbol d} [deg.]"
set yrange[0:180]
set ytics (0,45,90,135,180)
set size 1,0.3
set origin 0,0.6
set tmargin 2
set bmargin 0
set lmargin 10
set rmargin 50
p "phs_p3.d" t "" w l lt 3 lw 6.0 

set nox2label 
set ylabel "{/Symbol D}(E) [MeV^{-1}]"
set yrange[-0.3:1.5]
set ytics (0,0.5,1)
set size 1,0.25
set origin 0,0.35
set tmargin 0
set bmargin 0
set lmargin 10
set rmargin 50
p "cld_p3.d" t "" w l lt 3 lw 6.0 

set ylabel "Im-Energy [MeV]"
set xlabel "Re-Energy [MeV]"
set nox2label
set yrange[-8:0]
set xtics 0,1,5
set ytics (-8,-6,-4,-2)
#set arrow from 0.78,-8 to 0.78,16 nohead lt 1 lw 3
set size 1,0.25
set origin 0,0.1
set tmargin 0
set bmargin 0
set lmargin 10
set rmargin 50
p "ene_p3.d" u 2:3 t "" w p lt 3 pt 7 ps 1.3

unset multiplot
