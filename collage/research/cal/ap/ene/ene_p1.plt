#!/usr/bin/gnuplot -persist
PS = 0
PS = 1
if (PS == 1) set term postscript eps enhanced color 30 solid
if (PS == 1) set output "ene_p1.eps"
set xrange [0:5]                              
set x2range [0:5]                              
set yrange [-6:0]
set y2range [-6:0]
unset key
set xlabel "Re-energy[MeV]"                   
set label "(E_{r},{/Symbol G})=(3.08,6.08)" at 3.4,-2.5 center
set ylabel "Im-energy[MeV]"                   
p "data/ene_p1.d" u 2:3 t "p_{1/2}" w p lt 3 pt 7 ps 3,\
  "data/pole_p1.txt" u "" w p lt 1 pt 7 ps 3
if (PS == 0) pause -1

