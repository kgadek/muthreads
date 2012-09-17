set xlabel "Conns"
set ylabel "Time"
set logscale x
set style data lines

set terminal png large
set output 'lol.png'

plot 'T.dat' title 'rotfl'
#w errorbars
