# LEVPLT --- LEVel data to gnuPLoT input file converter
# File Name:c12-lev.plt
# Input   Level    File =c12-lev.dat
# Output  Set      File =c12-lev.set
# Output  Data     File =c12-lev.lev
# Output  Plot     File =c12-lev.plt
load "c12-lev.set"                                                      
set noxtics
set nokey
set xrange [   0.500:  27.000]
plot "c12-lev.lev" with lines                                           
pause -1 "(Return to continue)"
clear
set term post enh "Helvetica" 20
#set term post portrait "Times"
set output "c12-lev.ps"
replot
