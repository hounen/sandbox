#!/usr/bin/gnuplot -persist
set xrange [0:50]
set x2range [0:50]
set yrange [-160:]
set y2range [-160:0]
set xlabel "Re-energy[MeV]"
set x2label "E_{r}=17.8MeV {/Symbol G}=64.3MeV"
set ylabel "Im-energy[MeV]"
set term postscript eps color enhanced 30 solid
set output "ene_f5.eps"
p "ene_f5.d" u 2:3 t "f_{5/2}" w p lt 3 pt 7 ps 3

