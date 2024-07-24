#!/usr/bin/gnuplot -persist
PS = 0
PS = 1
if (PS == 1) set term postscript eps enhanced color 30 solid
if (PS == 1) set output "ene_k1_j9.eps"
set xrange [0:40]                              
set x2range [0:40]                              
set yrange [-20:0]
set y2range [-20:0]
set xlabel "Re-energy[MeV]"                   
set ylabel "Im-energy[MeV]"                   
set label "(E_{r},{/Symbol G})=(18.47,1.72)" at 18.47,-3 left
set label "(E_{r},{/Symbol G})=(23.7,18.94)" at 20.0,-11.00 left
set x2tics (" " 20)
p "data/analysis/ene_k1^4_j9.d"\
  u 2:3 t "J^{/Symbol p}=9^{-}" w p lt 3 pt 7 ps 3,\
  "data/analysis/pole_1_j9.txt" t "" w p lt 1 pt 7 ps 3,\
  "data/analysis/pole_2_j9.txt" t "" w p lt 1 pt 7 ps 3
if (PS == 0) pause -1

