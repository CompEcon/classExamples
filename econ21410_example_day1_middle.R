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
library(ggplot2)

#==================
# Section 1: The easy problem:
#==================

#  max U(X,Y) s.t. p_x X + p_y Y = M

#-----------------
# Section 1.1: Defining utility function
#-----------------

UtilSimple <- function(x, alpha = .3, gamma =.5, util.type = "cobb", p.x = p.x, p.y = p.y, M) {
  # Calculates the utility given x and y.
  #
  # Args: x = units of the two goods consumed. 
  #       util.type = c("cobb","leon"), alpha = the cobb douglas parameter
  #       p.x, p.y, and M are defined in the budget constraint function
  #
  # Returns: The utility for a given bundle (x,y) returned as a single number
  # Error Handling;
  if (class(x) != "numeric" )
    stop("Argument x must be numeric")
  if (length(x) != 2)
    stop("Argument x must have dimension 2")
  # defining two possible utility functions
  if (util.type == "cobb") {
    util <- x[1]^alpha * x[2] ^gamma
  }
  if (util.type == "leon") {
    util = min(x)
    if (x[1]< 0 | x[2] < 0)
      util <- -100000
    if (M - p.x*x[1] - p.y*x[2] < 0 )
      util <-  -100000
  }
    return(-1 * util)
}

BudgetConst <- function(x, M, p.x = p.x, p.y = p.y, alpha = .3, gamma=.5, util.type = "cobb") {
  #Calculates if the budget constraint holds for a given bundle, prices, and income
  # 
  #Args: x is a list of x and y quantities. p.x is price of x[1] p.y is price of x[2]
  #      M is income. Ther other args defineed in the outcomes
  #
  # Returns: the negative budget surplus
  # Error Handling:
  if (class(x) != "numeric" )
    stop("Argument x must be numeric")
  if (length(x) != 2)
    stop("Argument x must have dimension 2")
  surplus <- M - p.x*x[1] - p.y*x[2]
  return(  surplus)
}


#-----------------
# Section 1.2: Defining parameters and true solutino
#-----------------

# defining parameters
alpha  <- .3
gamma  <- .5
M      <- 100
p.x    <- 2
p.y    <- 5
x.star <-  c ( (alpha /(( alpha + gamma) * p.x)) * M , (gamma /(( alpha + gamma) * p.y)) * M )

# solving with 
sol.simple <- solnp(pars = c(1,1), fun=UtilSimple, ineqfun=BudgetConst, ineqLB=0, ineqUB=Inf,
             LB=c(0,0), p.x = p.x, p.y = p.y, alpha = alpha, gamma = gamma, M= M , util.type="cobb")


# Solving wiht a global optimizer for leontief
sol.simplealt <- nloptr(x0=c(1,1), eval_f=UtilSimple, lb=c(0,0), ub=c(50,20),
   util.type="leon", alpha=alpha, p.x=p.x, p.y=p.y, M=M, gamma = gamma,
   opts= list("algorithm"="NLOPT_GN_ISRES", "maxtime"=5,"maxeval"=800000))

x.star
sol.simple$par
sol.simplealt$solution

