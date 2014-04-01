#=======================================================
#
#  Econ 21410 - In Class Example
#
#  date: March 30, 2014
#  author: JOhn Eric HUmphries
#
#  abstract: A review of how computers solve problems
#            and good coding practices
#
#========================================================


#==================
# Section 0: setup
#==================
rm(list=ls())
set.seed(111)

#libraries
library(nloptr)
library(Rsolnp)


#==================
# Section 1: The easy problem:
#==================

#  max U(X,Y) s.t. p_x X + p_y Y = M

#-----------------
# Section 1.1: Defining utility function
#-----------------

UtilSimple <- function(x, alpha = .3, gamma =.5, p.x = p.x, p.y = p.y, M = M) {
  # Calculates the utility given x and y.
  #
  # Args: x = units of the two goods consumed. 
  #        alpha and gamma = the cobb douglas parameters
  #       p.x, p.y, and M are defined in the budget constraint function
  #
  # Returns: The utility for a given bundle (x,y) returned as a single number
  util <- x[1]^alpha * x[2] ^gamma
  if (x[1]< 0 | x[2] < 0)
    util <- -100000
  if (M - p.x*x[1] - p.y*x[2] < 0 )
    util <-  -100000
  return(-1 * util)
}


#-----------------
# Section 1.2: Defining parameters and true solution
#-----------------

# defining parameters
alpha  <- .3
gamma  <- .5
M      <- 100
p.x    <- 2
p.y    <- 5

# analytic solution
x.star <-  c ( (alpha /(( alpha + gamma) * p.x)) * M , (gamma /(( alpha + gamma) * p.y)) * M )

#-----------------
# Section 1.3: solving
#-----------------

# Solving wiht a global optimizer for leontief
sol.simplealt <- nloptr(x0=c(1,1), eval_f=UtilSimple, lb=c(0,0), ub=c(50,20), alpha=alpha, gamma=gamma, p.x=p.x, p.y=p.y, M=M,
   opts= list("algorithm"="NLOPT_GN_CRS2_LM", "maxtime"=5,"maxeval"=800000))

# results
sol.simplealt$solution
x.star