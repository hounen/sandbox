#!/usr/bin/gnuplot - persist
set term postscript enh color solid
set output "phs_nn_pp.eps"
set xrange [0:200]
set yrange [-20:70]
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
p "data/nn/analysis/nnphs02.d" u 1:2 t "nn" w l lt 3 lw 5,\
  "data/pp/analysis/ppphs02.d" u 1:2 t "pp" w l lt 1 lw 5,\
  0 t "" lt 0 lw 3
  
