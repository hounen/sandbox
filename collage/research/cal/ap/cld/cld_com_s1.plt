#!/usr/bin/gnuplot -persist
set xrange [0:1]
set yrange [-0.5:0.5]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol D}(E)[MeV^{-1}]"
set terminal postscript eps enhanced color 30 solid
set output "cld_com_s1.eps"        
p "../../helium5/cld/cld_s1.d" t "he5-s_{1/2}" w l lw 6.0,\
  "li5_cld_s1.d" u 1:2 t "li5-s_{1/2}" w l lw 6.0

