#!/usr/bin/gnuplot -persist
set xrange [0:100]
set yrange [-0.01:0.02]
set xlabel "Energy[Mev]"
set ylabel "{/Symbol D}(E)[MeV^{-1}]"
set x2tics ("E_{r}" 17.84957)
set grid x2tics lt -1 lw 3
set terminal postscript eps enhanced color 30 solid
set output "allcld_f5_100MeV.eps"
p "allcld_f5_100MeV.d" u 1:2 t "f_{5/2}" w l lw 6.0 ,\
  "allcld_f5_100MeV.d" u 1:3 t "resonance" w l lw 6.0 ,\
  "allcld_f5_100MeV.d" u 1:4 t "continuum" w l lw 6.0
