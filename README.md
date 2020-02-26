# Modified Lab #6
######################################################
# Co-authors:
# Patrick Applegate, patrick.applegate@psu.edu
# Klaus Keller, klaus@psu.edu
# Edited for Use in Problem Set 1:
# Angelina Santamaria, aqs6427@psu.edu
###################################################
# demonstrate a simple bootstrap analyis of coin flip data
#######################################################
# Copyright 2020 by the Authors
##########################################################
# last changes:
# -> Modification for Problem Set 1 on 2/20/2020; aqs6427@psu.edu
###########################################################

# This file is part of Risk Analysis in the Earth Sciences: A Lab Manual with 
# Exercises in R.

# This e-textbook is free software: you can redistribute it and/or modify it 
# under the terms of the GNU General Public License as published by the Free 
# Software Foundation, either version 3 of the License, or (at your option) 
# any later version.

# This e-textbook is distributed in the hope that it will be useful, but 
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for 
# more details.

# You should have received a copy of the GNU General Public License along with 
# this e-textbook.  If not, see <http://www.gnu.org/licenses/>.
#
# Start the analysis
#######################
# Clear away any existing variables or figures.  
rm(list = ls())
graphics.off()

#  step 1, use a simple Monte Carlo Simulation of Coin Flips
# Perform a simulation of successive estimates of c, the ratio of heads to the
# total number of coin flips, for a fair coin.

# Set some values.
true.c <- 0.5 
# the true ratio of heads to total flips (0.5 indicates a fair coin)

n.flips <- 5* 10^ 2 
# how many coin flips to perform

# Perform the random sampling and calculate the estimates of c. 1, heads; # 0, tails.
set.seed(1)
ht <- sample(x = c(0, 1), size = n.flips, replace = TRUE,
             prob = c(true.c, 1- true.c))

flips <- seq(from = 1, to = n.flips, by = 1)
c.ests <- cumsum(ht)/ flips

# How stable is the estimate as a function of MC runs?
# Plot up the results.
pdf('Estimate Function as MC of Runs.pdf', width=10,height=10/1.618)
plot(flips, c.ests, type =  'l' , col =  'blue' , log =  'x' ,
     xlab =  'Number of flips carried out (log scale)' , ylab =  'Estimates of c' )
abline(h = true.c, lty = 2)
dev.off()


# Step 2: Use a bootstrap anaysis of one real data
#####################################################
# Set up a vector of coin flips that represents the data.  1, heads; 0, tails.
ht.data <- c(1, 0, 1, 1, 0, 1, 1, 0, 0, 0) 
# data provided through problem set

# Calculate the value of c based on ht.data.
c.data <- sum(ht.data)/ length(ht.data)
# Generate one bootstrap replicate based on ht.data and calculate the # associated estimate of c.
set.seed(1)

ht.boot <- sample(x = ht.data, size = length(ht.data), replace = TRUE, prob = NULL)
c.boot <- sum(ht.boot)/ length(ht.boot)

n.boot = 10e4

bootstrap <- rep(NA,n.boot)
for(i in 1:n.boot) {
  ht.boot <- sample(x = ht.data, size = length(ht.data), replace = TRUE, prob = NULL)
  c.boot <- sum(ht.boot)/ length(ht.boot)
  bootstrap[i] <- c.boot
}

require(arm)

# What do you expect the variation to be of this data set? 
pdf('Problem Set #1 Bootstrap.pdf ', width=10,height=10/1.618)
discrete.hist(bootstrap, bar.width=0.09, prob.col='light blue')
abline(v = true.c , col="green", lwd=3, lty=1)
abline(v = c.boot , col="black", lwd=3, lty=2)
dev.off()


# do a bit more complex plot
pdf('lab#6_plot3.pdf', width=10,height=10/1.618)
h<-hist(bootstrap, breaks=c(unique(bootstrap)-0.05,max(bootstrap)+0.05), col="light blue", xlab="Estimate of c", ylab = "Frequency of c",  main= "Distribution of c Frequencies", freq=T)
abline(v = true.c , col="green", lwd=3, lty=1)
abline(v = c.boot , col="black", lwd=3, lty=2)
dev.off()
