#!/usr/bin/gnuplot -persist
set xrange [0:1]
set yrange [-0.5:0.5]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol D}(E)[MeV^{-1}]"
set terminal postscript eps enhanced color 30 solid
set output "cld_com_he5_li5_s.eps"        
p "../helium5/cld/cld_s1.d" u 1:2 t "^5He s_{1/2}" w l lt 0 lw 8.0,\
  "../lithium5/cld/data/cld_s1.d" u 1:2 t "^5Li s_{1/2}" w l lt -1 lw 2.0
