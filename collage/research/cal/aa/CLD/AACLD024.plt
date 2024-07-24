#!/usr/bin/gnuplot -persist
set xrange[0:20] 
set yrange[-0.5:1]
set xtics 0,2,20
set ytics -0.5,0.5,1
set xlabel "Energy [MeV]"
set ylabel "{/Symbol D}(E)[MeV^{-1}]"
set terminal postscript enhanced color 30 solid
set output "AACLD024.ps"
p  "DATA/AACLD0.d" u 1:2 t "L=0" w l lw 6.0,\
   "DATA/AACLD2.d" u 1:2 t "L=2" w l lw 6.0,\
   "DATA/AACLD4.d" u 1:2 t "L=4" w l lw 6.0,\
  90 t "" w l 0
