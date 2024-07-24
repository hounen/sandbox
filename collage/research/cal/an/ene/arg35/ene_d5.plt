#!/usr/bin/gnuplot -persist
set xrange [0:40]
set x2range [0:40]
set yrange [-120:0]
set y2range [-120:0]
set xlabel "Re-energy[MeV]"
set x2label "E_{r}=28.5MeV {/Symbol G}=43.1MeV" 
set ylabel "Im-energy[MeV]"
set term postscript eps color enhanced 30 solid
set output "ene_d5.eps"
p "ene_d5.d" u 2:3 t "d_{5/2}" w p lt 3 pt 7 ps 3 

