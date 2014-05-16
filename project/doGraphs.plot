# Create PDF images as output.
# usage: gnuplot doGraphs.plot

# General output settings.
set terminal postscript enhanced color font 'Helvetica,18' linewidth 2
set encoding default

set ylabel "MSE"
set xlabel "Cummulative assignments"
set mxtics
set mytics

NAME = "Linear_regression_on_MOOC"
FILENAME = "final_grade_gnuplot.txt"
set title "Linear regression on MOOC"
#set autoscale y
#set yrange [0.997:1]

set output "| ps2pdf - ".NAME.".pdf"
plot FILENAME using 1:2 \
    title "All imputed" lt 1 lc rgb "#EE0000" lw 2 with lines

set output "| ps2pdf - ".NAME.".pdf"
replot FILENAME using 1:3 \
    title "Imputed data with finals" lt 1 lc rgb "#00EE00" lw 2 with lines

set output "| ps2pdf - ".NAME.".pdf"
replot FILENAME using 1:4 \
    title "Original data with finals" lt 1 lc rgb "#0000EE" lw 2 with lines


NAME = "Num_students"
FILENAME = "num_students.txt"
set title "Number of students that completed each assignment"
set ylabel "# students"
set xlabel "# assignment"

set output "| ps2pdf - ".NAME.".pdf"
plot FILENAME using 1:2 \
    title "" lt 1 lc rgb "#FF0000" lw 2 with lines
