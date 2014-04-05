#===========================================
#
#  TITLE:  computational economics: example knitr file
#  AUTHOR: John Eric Humphries
#  abstract: an example of a knitr file which uses on outside .R file
#
#  Date: 2014-04-05
#
#===========================================


#========================
## @knitr code_part0
# Section 0: setup
#========================

#setwd("/mnt/ide0/home/johneric/sbox/projects/neighborhoodVision/")
rm(list=ls())           # Clear the workspace
set.seed(907) 

library(xtable)         # A simple way to make latex tables


#================================
## @knitr code_part1
# Section 1: Generating Data
#================================

a = runif(100)
b = runif(100)
c = 12.345

x       = matrix(rnorm(200, mean = 1),100,2)
epsilon = rnorm(100)
y       = 1 + 2*x[,1] -3*x[,2] + epsilon



#================================
## @knitr code_part2
# Section 2: stuff with data
#================================

mean(a + b)
obs = length(a)
class(b)


#================================
## @knitr code_part3
# Section 3: stuff with data
#================================

# saving regression results
y.reg = lm(y ~ x)

# displaying regression results
summary(y.reg)

# making table of regression results
## @knitr code_part4
xtable(y.reg, caption="A TABLE OF REGRESSION RESULTS")
## @knitr code_part4_

#================================
## @knitr code_part5
# Section 4: displaying a plot
#================================

plot(a,b)


