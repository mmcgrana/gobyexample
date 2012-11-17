#
# $Id: prob2.dem,v 1.9 2006/06/14 03:24:09 sfeam Exp $
#
# Demo Statistical Approximations version 1.1
#
# Copyright (c) 1991, Jos van der Woude, jvdwoude@hut.nl

# History:
#    -- --- 1991 Jos van der Woude:  1st version
#    06 Jun 2006 Dan Sebald:  Added plot methods for better visual effect.

print ""
print ""
print ""
print ""
print ""
print ""
print "                        Statistical Approximations, version 1.1"
print ""
print "        Copyright (c) 1991, 1992, Jos van de Woude, jvdwoude@hut.nl"
print ""
print ""
print ""
print ""
print ""
print ""
print ""
print ""
print ""
print ""
print ""
print "     NOTE: contains 10 plots and consequently takes some time to run"
print "                      Press Ctrl-C to exit right now"
print ""
pause -1 "                      Press Return to start demo ..."

load "stat.inc"
rnd(x) = floor(x+0.5)
r_xmin = -1
r_sigma = 4.0

# Binomial PDF using normal approximation
n = 25; p = 0.15
mu = n * p
sigma = sqrt(n * p * (1.0 - p))
xmin = floor(mu - r_sigma * sigma)
xmin = xmin < r_xmin ? r_xmin : xmin
xmax = ceil(mu + r_sigma * sigma)
ymax = 1.1 * binom(floor((n+1)*p), n, p) #mode of binomial PDF used
set key box
unset zeroaxis
set xrange [xmin - 1 : xmax + 1]
set yrange [0 : ymax]
set xlabel "k, x ->"
set ylabel "probability density ->"
set ytics 0, ymax / 10.0, ymax
set format x "%2.0f"
set format y "%3.2f"
set sample 200
set title "binomial PDF using normal approximation"
set arrow from mu, 0 to mu, normal(mu, mu, sigma) nohead
set arrow from mu, normal(mu + sigma, mu, sigma) \
          to mu + sigma, normal(mu + sigma, mu, sigma) nohead
set label "mu" at mu + 0.5, ymax / 10
set label "sigma" at mu + 0.5 + sigma, normal(mu + sigma, mu, sigma)
plot binom(rnd(x), n, p) with histeps, normal(x, mu, sigma)
pause -1 "Hit return to continue"
unset arrow
unset label

# Binomial PDF using poisson approximation
n = 50; p = 0.1
mu = n * p
sigma = sqrt(mu)
xmin = floor(mu - r_sigma * sigma)
xmin = xmin < r_xmin ? r_xmin : xmin
xmax = ceil(mu + r_sigma * sigma)
ymax = 1.1 * binom(floor((n+1)*p), n, p) #mode of binomial PDF used
set key box
unset zeroaxis
set xrange [xmin - 1 : xmax + 1]
set yrange [0 : ymax]
set xlabel "k ->"
set ylabel "probability density ->"
set ytics 0, ymax / 10.0, ymax
set format x "%2.0f"
set format y "%3.2f"
set sample (xmax - xmin + 3)
set title "binomial PDF using poisson approximation"
set arrow from mu, 0 to mu, normal(mu, mu, sigma) nohead
set arrow from mu, normal(mu + sigma, mu, sigma) \
          to mu + sigma, normal(mu + sigma, mu, sigma) nohead
set label "mu" at mu + 0.5, ymax / 10
set label "sigma" at mu + 0.5 + sigma, normal(mu + sigma, mu, sigma)
plot binom(x, n, p) with histeps, poisson(x, mu) with histeps
pause -1 "Hit return to continue"
unset arrow
unset label

# Geometric PDF using gamma approximation
p = 0.3
mu = (1.0 - p) / p
sigma = sqrt(mu / p)
lambda = p
rho = 1.0 - p
xmin = floor(mu - r_sigma * sigma)
xmin = xmin < r_xmin ? r_xmin : xmin
xmax = ceil(mu + r_sigma * sigma)
ymax = 1.1 * p
set key box
unset zeroaxis
set xrange [xmin - 1 : xmax + 1]
set yrange [0 : ymax]
set xlabel "k, x ->"
set ylabel "probability density ->"
set ytics 0, ymax / 10.0, ymax
set format x "%2.0f"
set format y "%3.2f"
set sample 200
set title "geometric PDF using gamma approximation"
set arrow from mu, 0 to mu, gmm(mu, rho, lambda) nohead
set arrow from mu, gmm(mu + sigma, rho, lambda) \
          to mu + sigma, gmm(mu + sigma, rho, lambda) nohead
set label "mu" at mu + 0.5, ymax / 10
set label "sigma" at mu + 0.5 + sigma, gmm(mu + sigma, rho, lambda)
plot geometric(rnd(x),p) with histeps, gmm(x, rho, lambda)
pause -1 "Hit return to continue"
unset arrow
unset label

# Geometric PDF using normal approximation
p = 0.3
mu = (1.0 - p) / p
sigma = sqrt(mu / p)
xmin = floor(mu - r_sigma * sigma)
xmin = xmin < r_xmin ? r_xmin : xmin
xmax = ceil(mu + r_sigma * sigma)
ymax = 1.1 * p
set key box
unset zeroaxis
set xrange [xmin - 1 : xmax + 1]
set yrange [0 : ymax]
set xlabel "k, x ->"
set ylabel "probability density ->"
set ytics 0, ymax / 10.0, ymax
set format x "%2.0f"
set format y "%3.2f"
set sample 200
set title "geometric PDF using normal approximation"
set arrow from mu, 0 to mu, normal(mu, mu, sigma) nohead
set arrow from mu, normal(mu + sigma, mu, sigma) \
          to mu + sigma, normal(mu + sigma, mu, sigma) nohead
set label "mu" at mu + 0.5, ymax / 10
set label "sigma" at mu + 0.5 + sigma, normal(mu + sigma, mu, sigma)
plot geometric(rnd(x),p) with histeps, normal(x, mu, sigma)
pause -1 "Hit return to continue"
unset arrow
unset label

# Hypergeometric PDF using binomial approximation
nn = 75; mm = 25; n = 10
p = real(mm) / nn
mu = n * p
sigma = sqrt(real(nn - n) / (nn - 1.0) * n * p * (1.0 - p))
xmin = floor(mu - r_sigma * sigma)
xmin = xmin < r_xmin ? r_xmin : xmin
xmax = ceil(mu + r_sigma * sigma)
ymax = 1.1 * hypgeo(floor(mu), nn, mm, n) #mode of binom PDF used
set key box
unset zeroaxis
set xrange [xmin - 1 : xmax + 1]
set yrange [0 : ymax]
set xlabel "k ->"
set ylabel "probability density ->"
set ytics 0, ymax / 10.0, ymax
set format x "%2.0f"
set format y "%3.2f"
set sample (xmax - xmin + 3)
set title "hypergeometric PDF using binomial approximation"
set arrow from mu, 0 to mu, binom(floor(mu), n, p) nohead
set arrow from mu, binom(floor(mu + sigma), n, p) \
          to mu + sigma, binom(floor(mu + sigma), n, p) nohead
set label "mu" at mu + 0.5, ymax / 10
set label "sigma" at mu + 0.5 + sigma, binom(floor(mu + sigma), n, p)
plot hypgeo(x, nn, mm, n) with histeps, binom(x, n, p) with histeps
pause -1 "Hit return to continue"
unset arrow
unset label

# Hypergeometric PDF using normal approximation
nn = 75; mm = 25; n = 10
p = real(mm) / nn
mu = n * p
sigma = sqrt(real(nn - n) / (nn - 1.0) * n * p * (1.0 - p))
xmin = floor(mu - r_sigma * sigma)
xmin = xmin < r_xmin ? r_xmin : xmin
xmax = ceil(mu + r_sigma * sigma)
ymax = 1.1 * hypgeo(floor(mu), nn, mm, n) #mode of binom PDF used
set key box
unset zeroaxis
set xrange [xmin - 1 : xmax + 1]
set yrange [0 : ymax]
set xlabel "k, x ->"
set ylabel "probability density ->"
set ytics 0, ymax / 10.0, ymax
set format x "%2.0f"
set format y "%3.2f"
set sample 200
set title "hypergeometric PDF using normal approximation"
set arrow from mu, 0 to mu, normal(mu, mu, sigma) nohead
set arrow from mu, normal(mu + sigma, mu, sigma) \
          to mu + sigma, normal(mu + sigma, mu, sigma) nohead
set label "mu" at mu + 0.5, ymax / 10
set label "sigma" at mu + 0.5 + sigma, normal(mu + sigma, mu, sigma)
plot hypgeo(rnd(x), nn, mm, n) with histeps, normal(x, mu, sigma)
pause -1 "Hit return to continue"
unset arrow
unset label

# Negative binomial PDF using gamma approximation
r = 8; p = 0.6
mu = r * (1.0 - p) / p
sigma = sqrt(mu / p)
lambda = p
rho = r * (1.0 - p)
xmin = floor(mu - r_sigma * sigma)
xmin = xmin < r_xmin ? r_xmin : xmin
xmax = ceil(mu + r_sigma * sigma)
ymax = 1.1 * gmm((rho - 1) / lambda, rho, lambda) #mode of gamma PDF used
set key box
unset zeroaxis
set xrange [xmin - 1 : xmax + 1]
set yrange [0 : ymax]
set xlabel "k, x ->"
set ylabel "probability density ->"
set ytics 0, ymax / 10.0, ymax
set format x "%2.0f"
set format y "%3.2f"
set sample 200
set title "negative binomial PDF using gamma approximation"
set arrow from mu, 0 to mu, gmm(mu, rho, lambda) nohead
set arrow from mu, gmm(mu + sigma, rho, lambda) \
          to mu + sigma, gmm(mu + sigma, rho, lambda) nohead
set label "mu" at mu + 0.5, ymax / 10
set label "sigma" at mu + 0.5 + sigma, gmm(mu + sigma, rho, lambda)
plot negbin(rnd(x), r, p) with histeps, gmm(x, rho, lambda)
pause -1 "Hit return to continue"
unset arrow
unset label

# Negative binomial PDF using normal approximation
r = 8; p = 0.4
mu = r * (1.0 - p) / p
sigma = sqrt(mu / p)
xmin = floor(mu - r_sigma * sigma)
xmin = xmin < r_xmin ? r_xmin : xmin
xmax = ceil(mu + r_sigma * sigma)
ymax = 1.1 * negbin(floor((r-1)*(1-p)/p), r, p) #mode of gamma PDF used
set key box
unset zeroaxis
set xrange [xmin - 1 : xmax + 1]
set yrange [0 : ymax]
set xlabel "k, x ->"
set ylabel "probability density ->"
set ytics 0, ymax / 10.0, ymax
set format x "%2.0f"
set format y "%3.2f"
set sample 200
set title "negative binomial PDF using normal approximation"
set arrow from mu, 0 to mu, normal(mu, mu, sigma) nohead
set arrow from mu, normal(mu + sigma, mu, sigma) \
          to mu + sigma, normal(mu + sigma, mu, sigma) nohead
set label "mu" at mu + 0.5, ymax / 10
set label "sigma" at mu + 0.5 + sigma, normal(mu + sigma, mu, sigma)
plot negbin(rnd(x), r, p) with histeps, normal(x, mu, sigma)
pause -1 "Hit return to continue"
unset arrow
unset label

# Normal PDF using logistic approximation
mu = 1.0; sigma = 1.5
a = mu
lambda = pi / (sqrt(3.0) * sigma)
xmin = mu - r_sigma * sigma
xmax = mu + r_sigma * sigma
ymax = 1.1 * logistic(mu, a, lambda) #mode of logistic PDF used
set key box
unset zeroaxis
set xrange [xmin: xmax]
set yrange [0 : ymax]
set xlabel "x ->"
set ylabel "probability density ->"
set ytics 0, ymax / 10.0, ymax
set format x "%.1f"
set format y "%.2f"
set sample 200
set title "normal PDF using logistic approximation"
set arrow from mu,0 to mu, normal(mu, mu, sigma) nohead
set arrow from mu, normal(mu + sigma, mu, sigma) \
          to mu + sigma, normal(mu + sigma, mu, sigma) nohead
set label "mu" at mu + 0.5, ymax / 10
set label "sigma" at mu + 0.5 + sigma, normal(mu + sigma, mu, sigma)
plot logistic(x, a, lambda), normal(x, mu, sigma)
pause -1 "Hit return to continue"
unset arrow
unset label

# Poisson PDF using normal approximation
mu = 5.0
sigma = sqrt(mu)
xmin = floor(mu - r_sigma * sigma)
xmin = xmin < r_xmin ? r_xmin : xmin
xmax = ceil(mu + r_sigma * sigma)
ymax = 1.1 * poisson(mu, mu) #mode of poisson PDF used
set key box
unset zeroaxis
set xrange [xmin - 1 : xmax + 1]
set yrange [0 : ymax]
set xlabel "k, x ->"
set ylabel "probability density ->"
set ytics 0, ymax / 10.0, ymax
set format x "%2.0f"
set format y "%3.2f"
set sample 200
set title "poisson PDF using normal approximation"
set arrow from mu, 0 to mu, normal(mu, mu, sigma) nohead
set arrow from mu, normal(mu + sigma, mu, sigma) \
          to mu + sigma, normal(mu + sigma, mu, sigma) nohead
set label "mu" at mu + 0.5, ymax / 10
set label "sigma" at mu + 0.5 + sigma, normal(mu + sigma, mu, sigma)
plot poisson(rnd(x), mu) with histeps, normal(x, mu, sigma)
pause -1 "Hit return to continue"
reset
