#!/usr/bin/gnuplot -persist
p "ene_p1.d" u 2:3 t "p_{1/2}"
set xrange [0:2]
set xlabel "Re-energy[MeV]"
set ylabel "Im-energy[MeV]"
rep
set term postscript eps enhanced 30 solid
set output "ene_p1.eps"
rep
