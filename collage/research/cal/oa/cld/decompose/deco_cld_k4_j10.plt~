#!/usr/bin/gnuplot -persist
set terminal postscript eps enhanced color 30 solid
set xrange [0:40]
set yrange [-0.5:0.5]
set xlabel "Energy [MeV]"
set ylabel "{/Symbol D} [MeV^{-1}]"
set x2tics ("E_{r}" 26.39)
set arrow 1 from 26.39,-0.5 rto 26.39,0.5 nohead lt -1 lw 3 
set output "deco_cld_k4_j10.eps"
p "../data/cld_k4_j10.d" u 1:2 t "K^{/Symbol p}=0_{4}^{+} J^{/Symbol p}=10^{+}" w l lw 6.0,\
  "../data/cld_k4_j10.d" u 1:3 t "resonance" w l lw 6.0,\
  "../data/cld_k4_j10.d" u 1:4 t "continuum" w l lw 6.0,\
  90 t "" w l 0

