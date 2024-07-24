#!/usr/bin/gnuplot -persist
set term postscript eps enhanced color 30 solid
set output "ene_s1.eps"
set xrange [0:5]
set xlabel "Re-energy[MeV]"
set ylabel "Im-energy[MeV]"
p "data/ene_s1.d" u 2:3 t "s_{1/2}" w p lt 3 pt 7 ps 3  
