
#===========================================
#
#  computational economics: week 2 day 2 class example
#  John Eric HUmphries
#  2014-04-09
#
# abstract: for students to study as part of their hw.
#
#===========================================


#========================
# Section 0: setup
#========================
rm(list=ls()) 
library(ggplot2)

#========================
# Section 1: control flow
#========================

# What does this loop display?
for (i in 1:10) {
    if ( i%%2 == 0) {
        i = i + 1    
    } else if (i%%3==0) {
        i = i*10
    } else {
        i = -1
    } 
    print(i)
}
# we need to not forget= versus =.

# What does this loop display?
k = rep(0,10)
for (i in 1:10) {
    for (j in 1:10) {
    k[i] = k[i] + j
    }
}
# what if i wanted to not add the number 1 to k[1], the number 2 to k[2] etc?

#==============================
# Section 3: A land-price model
#==============================

LandPrice <- function(n1,n2,p1,p2) {
    # This function simulates a location model based on income.
    #
    # Args: n1 - the number of group1 to simulate
    #       n2 - the number of group2 to simulate
    #       k - the means of their incomes
    #       p1 and p2 - tuning parameters for the distributions
    # Output: The final distribution of ppl an types (plots as side effects)
    # Error Handling:
    if (p1<0 | p2<0)
        stop("p1 and p2 must be positive")
    n = n1 + n2
    data <- matrix(NA,sum(n),4)
    data[,2:3] <- runif(n*2)
    data[1:n1,4]  = 1
    data[(n1+1):n,4]  = 0
    data[1:n1,1]  = rbeta(n1,2,shape2=p1)
    data[(n1+1):n,1]  = rbeta(n2,2,shape2=p2)
    colnames(data) <- c("income","x","y","color")
    for (ind in 1:n) {
        take = 0
        while (take == 0) {
            data[ind,2:3] = runif(2)
            cost   = sqrt(data[ind,2]^2 + data[ind,3]^2)
            bound  = abs(rnorm(1,sd=.25))
            take   = cost - bound < data[ind,1] &  cost + bound >= data[ind,1] 
        }
    }
    Legend = factor(data[,4],levels=c(0,1),labels=c("Reds","Blues"))
    plot1 <- (qplot(x=data[,2], y=data[,3], geom="point",  color=Legend,size=I(3),
                    xlab="", ylab="")   + theme( legend.position="bottom", legend.text=element_text(size=14), legend.title=element_blank())
              + ggtitle("Land Price Model"))
    print(plot1)
    return(data)
}

data = LandPrice(200,200,15,1)

