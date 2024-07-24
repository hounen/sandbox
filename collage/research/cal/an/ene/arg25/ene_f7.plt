#!/usr/bin/gnuplot -persist
p "ene_f7.d" u 2:3 t "f_{7/2}" 
set xrange [0:2]
set xlabel "Re-energy[MeV]"
set ylabel "Im-energy[MeV]"
rep
set term postscript eps enhanced 30 solid
set output "ene_f7.eps"
rep

