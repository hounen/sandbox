#!/usr/bin/gnuplot -persist
set xrange [0:50]
set xlabel "Re-energy[MeV]"
set ylabel "Im-energy[MeV]"
set x2label "E_{r}=25.4MeV {/Symbol G}=55.7MeV"
set term postscript eps color enhanced 30 solid
set output "ene_f7.eps"
p "ene_f7.d" u 2:3 t "f_{7/2}" w p lt 3 pt 7 ps 3

