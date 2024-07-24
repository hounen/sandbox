#!/usr/bin/gnuplot -persist
set xrange [0:50]
set x2range [0:50]
set yrange [-140:0]
set y2range [-140:0]
set xlabel "Re-energy[MeV]"
set x2label "E_{r}=29.3MeV  {/Symbol G}=37.6MeV"
set ylabel "Im-energy[MeV]"
set term postscript eps color enhanced 30 solid
set output "ene_d3.eps"
p "ene_d3.d" u 2:3 t "d_{3/2}" w p lt 3 pt 7 ps 3 

