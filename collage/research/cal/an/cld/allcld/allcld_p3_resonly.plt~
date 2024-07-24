#!/usr/bin/gnuplot -persist
set xrange [0:10]
set yrange [-0.1:1.2]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol D}(E)[MeV^{-1}]"
#set x2tics ("E_{r}" 0.7846474)
#set grid x2tics lt -1 lw 3
set terminal postscript eps enhanced color 30 solid
set output "allcld_p3_resonly.eps"
p "allcld_p3_100MeV.d" u 1:2 t "p_{3/2} total" w l lt -1 lw 4.0 ,\
  "allcld_p3_100MeV.d" u 1:3 t "resonance" w l lt 1 lw 6.0

