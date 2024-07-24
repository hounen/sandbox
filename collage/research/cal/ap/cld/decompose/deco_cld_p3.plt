#!/usr/bin/gnuplot -persist
set xrange [0:10]
set yrange [-0.2:1.0]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol D}(E)[MeV^{-1}]"
set x2tics ("E_{r}" 1.60)
set grid x2tics lt -1 lw 3
set terminal postscript eps enhanced color 30 solid
set output "deco_cld_p3.eps"
p "../data/cld_p3.d" u 1:2 t "p_{3/2}" w l lw 6.0 ,\
  "../data/cld_p3.d" u 1:3 t "resonance" w l lw 6.0 ,\
  "../data/cld_p3.d" u 1:4 t "continuum" w l lw 6.0
