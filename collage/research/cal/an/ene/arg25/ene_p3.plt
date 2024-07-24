#!/usr/bin/gnuplot -persist
p "ene_p3.d" u 2:3 t "p_{3/2}"
set xrange [0:2]
set xlabel "Re-energy[MeV]"
set ylabel "Im-energy[MeV]"
set term postscript eps enhanced 30 solid
set output "ene_p3.eps"
rep

