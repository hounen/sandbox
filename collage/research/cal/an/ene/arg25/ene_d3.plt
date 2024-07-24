#!/usr/bin/gnuplot -persist
p "ene_d3.d" u 2:3 t "d_{3/2}" 
set xrange [0:2]
set xlabel "Re-energy[MeV]"
set ylabel "Im-energy[MeV]"
rep
set term postscript eps enhanced 30 solid
set output "ene_d3.eps"
rep

