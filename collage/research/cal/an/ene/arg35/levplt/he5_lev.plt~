#!/usr/bin/gnuplot -persist
set term postscript eps enhanced color "Times-Bold" 28
set output 'he5_lev.eps'
set border 10
unset xtics
set ytics 0,1,4
set ylabel 'Energy[MeV]'
set xrange [0:5]
set yrange [-0.5:4]
set label "Exp." at 1.5,-0.5 center 
set label "calc." at 3.5,-0.5 center
set label "{/Symbol G}=0.65" at 1.5,0.898 center
set label "{/Symbol G}=5.57" at 1.5,2.168 center
set label "{/Symbol G}=0.55" at 3.5,0.88 center
set label "{/Symbol G}=5.67" at 3.5,2.18 center
set label "3/2^-" at 0.5,0.898 left
set label "1/2^-" at 0.5,2.168 left 
set label "3/2^-" at 4.5,0.88  right 
set label "1/2^-" at 4.5,2.18  right
set label "{/Symbol a}-n threshold" at 0.8,0.2 center
#set zeroaxis
p 'he5_lev_exp.txt' u 1:($2+0.798) t "" w l lw 3 lt -1,\
  'he5_lev_ana.txt' t "" w l lw 3 lt -1,\
  'he5_lev_dif.txt' notitle  w l lw 3 lt 0,\
  0 t "" lt 0
