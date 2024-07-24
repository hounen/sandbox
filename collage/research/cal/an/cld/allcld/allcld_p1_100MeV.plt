#!/usr/bin/gnuplot -persist
set xrange [0:10]
set yrange [-0.2:0.2]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol D}(E)[MeV^{-1}]"
set terminal postscript eps enhanced color 30 solid
set x2tics ("E_{r}" 2.077087)
set grid x2tics lt -1 lw 3
set output "allcld_p1_100MeV.eps"
p "allcld_p1_100MeV.d" u 1:2 t "p_{1/2}" w l lw 6.0 ,\
  "allcld_p1_100MeV.d" u 1:3 t "resonance" w l lw 6.0 ,\
  "allcld_p1_100MeV.d" u 1:4 t "continuum" w l lw 6.0
