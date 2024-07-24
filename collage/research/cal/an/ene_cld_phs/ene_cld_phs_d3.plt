#!/usr/bin/gnuplot -persist
set term postscript eps enh color solid 
set output "ene_cld_phs_d3.eps"

set xrange [0:5]
unset xtics

#set format x""
set multiplot

set x2label "d_{3/2}   E_{r}=29.3MeV {/Symbol G}=37.6"
set ylabel "{/Symbol d} [deg.]"
set yrange[-5:20]
set ytics (0,45,90,135,180)
set size 1,0.3
set origin 0,0.6
set tmargin 2
set bmargin 0
set lmargin 10
set rmargin 50
p "phs_d3.d" t "" w l lt 3 lw 6.0

set nox2label
set ylabel "{/Symbol D}(E) [MeV^{-1}]"
set yrange[0:50]
set ytics (0,0.5,1)
set size 1,0.25
set origin 0,0.35
set tmargin 0
set bmargin 0
set lmargin 10
set rmargin 50
p "cld_d3.d" t "" w l lt 3 lw 6.0 

set ylabel "Im-Energy [MeV]"
set xlabel "Re-Energy [MeV]"
set xrange[0:50]
set yrange[-120:0]
set xtics 0,5,50
set nox2tics
set nox2label
set ytics (-120,-80,-40,-20)
#set arrow from 2.08,-8 to 2.08,16 nohead lt 1 lw 1
set size 1,0.25
set origin 0,0.1
set tmargin 0
set bmargin 0
set lmargin 10
set rmargin 50
p "ene_d3.d" u 2:3 t "" w p lt 3 pt 7 ps 1.3

unset multiplot

