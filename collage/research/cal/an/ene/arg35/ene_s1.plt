#!/usr/bin/gnuplot -persist
p "ene_s1.d" u 2:3 t "s_{1/2}" w p lt 3 pt 7 ps 3  
set xrange [0:2]
set xlabel "Re-energy[MeV]"
set ylabel "Im-energy[MeV]"
rep
set term postscript eps enhanced color 30 solid
set output "ene_s1.eps"
rep